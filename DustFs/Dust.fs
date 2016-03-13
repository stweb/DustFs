module Dust.Engine

open System
open System.IO
open System.Collections.Generic
open System.Collections.Concurrent
open System.Globalization
open System.Text.RegularExpressions

let rexKey   = new Regex("^[a-zA-Z_\$][0-9a-zA-Z_\-\$]*", RegexOptions.Compiled)
let rexFloat = new Regex("^(-)?[0-9]*(?:\.[0-9]*)?", RegexOptions.Compiled)
let rexInt   = new Regex("^[0-9]+", RegexOptions.Compiled)

let rexRefs  = new Regex("\{[^}]*\}", RegexOptions.Compiled)
let rexRefB  = new Regex("\[(.*?)\]", RegexOptions.Compiled)

type Identifier =
    | Key of string
    | Path of Segment list
with
    override x.ToString() =
        match x with
        | Key(k)  -> k
        | Path(p) -> String.Join(".", p |> List.map( fun x -> x.ToString() ))
    member x.ToPath() =
        match x with
        | Key(k)  -> [Name(k)]
        | Path(p) -> p

and Segment =
    | FromCur
    | Name of string
    | Index of int
with
    override x.ToString() =
        match x with
        | FromCur -> "°"
        | Name  s -> s
        | Index i -> sprintf "[%d]" i

type Value =
    | VInline of string
    | VIdent of Identifier
    | VNumber of decimal

type Logic =
    | Eq | Ne | Lt | Lte | Gt | Gte  // Logic Helper
//    | Sep | First | Last // Separator Helper
//    | Select | Any | None // Select Helper
//    | ContextDump // Debug Helper
    | Custom

type KeyValue = Map<string, Value>
type SectionType =
    | Scope // # - defines $idx and $len
    | Condition // ?name exists and evaluates to true
    | NotCondition // ^name doesn't exist or evaluates to false
    | Helper // @ helper method
    | LogicHelper of Logic // @ helper method
    | Inline // < inline partials
    | Block // + Blocks in the base template can contain default content and a child template can override that content.
    | Escaped // % - deprecated?

type Filter = obj -> obj
type Filters = Filter seq

type Body = Part list
and Part =
    | Buffer of string
    | Comment of string
    | Partial of Value * Identifier option * Params
    | Section of SectionType * Identifier * Identifier option * Params
    | SectionBlock of SectionType * Identifier * Identifier option * Params * Body * BodyDict
    | Special of char
    | Reference of Identifier * Filters
    | NamedBody of string
    | End of Identifier
    | Eol
    | Null

and BodyDict = Map<string, Body>
and Params   = Map<string, Value>

let filters = new ConcurrentDictionary<string, Filter>()

let cache = new ConcurrentDictionary<string, DateTime * Body>()
#if DEBUG
let names = new HashSet<string>()
#endif

/// Memoize asynchronous function. An item is recomputed when `isValid` returns `false`
let private asyncMemoize isValid f =
  fun x -> async {
      match cache.TryGetValue(x) with
      | true, res when isValid x res -> return res
      | _ ->
          let! res = f x
          cache.[x] <- res
          return res }

// trim quotes from a string - assumes proper double quoted
//let unquote (s:string) = if s.StartsWith("\"") then s.Substring(1, s.Length-2) else s

let elapsedMs (sw:System.Diagnostics.Stopwatch) =
    double sw.ElapsedTicks * 1000.0 / double System.Diagnostics.Stopwatch.Frequency

let inline toString chars = System.String(chars |> Array.ofList)

let (|Rex|_|) (rex:Regex) chars =
    let input = chars |> toString
    let m = rex.Match(input)
    if m.Success then Some (m.Value |> List.ofSeq, input.Substring(m.Length) |> List.ofSeq) else None

let (|StartChar|_|) ch (input:string) =
    if (input.Length > 1) && (input.[0] = ch) then Some (input.Substring 1) else None

let (|StartsWith|_|) prefix list =
    let rec loop = function
        | [], rest -> Some(rest)
        | p :: prefix, r :: rest when p = r -> loop (prefix, rest)
        | _ -> None
    loop (prefix, list)

let rec parseUntil closing acc = function
    | StartsWith closing rest -> Some(List.rev acc, rest)
    | c :: chars              -> parseUntil closing (c :: acc) chars
    | _ -> None

let (|Until|_|) closing chars =
    parseUntil closing [] chars

let (|BeforeEolOrTag|_|) chars = // [ '\n', '{' ]
    let rec loop acc = function
        | '{'  :: rest -> if acc <> [] then Some(List.rev acc, '{'  :: rest) else None
        | '\n' :: rest -> if acc <> [] then Some(List.rev acc, '\n' :: rest) else None
        | '\r' :: rest -> if acc <> [] then Some(List.rev acc, '\r' :: rest) else None
        | c :: chars   -> loop (c :: acc) chars
        | _ -> None
    loop [] chars

let (|Quoted|_|) = function
    | '\"' :: chars -> chars |> parseUntil ['\"'] []
    | _ -> None

// --------------------------------------------------------------------------------------
let parseFilters (strarr:seq<string>) =
    seq {
        for f in Seq.skip 1 strarr do
            let name = f.Trim()
            match filters.TryGetValue name with
            | true, filter -> yield filter
            | _ -> ()
    }

// inline <- '\"' '\"' / '\"' literal '\"'/ '\"' inline_part+ '\"'
// TODO inline_part <- special / reference / literal
let (|Inline|_|) = function
    | Quoted(x) -> Some x // TODO? literal vs inline_part
    | _         -> None

// key <- (({Ll} / {Lu} / [_$]) ({Nd} / {Ll} / {Lu} / [_$-])*)
// IMPORTANT RegEx must start with '^' to match from the start
let (|Key|_|) = function
    | Rex rexKey x -> Some x
    | _ -> None

let (|Int|_|) = function
    | Rex rexInt (i,r) -> Some(Convert.ToInt32(toString i),r)
    | _ -> None


let numOf x r =
    match Decimal.TryParse(toString x, NumberStyles.Any, CultureInfo.InvariantCulture) with
              | true, n -> Some (n, r)
              | _       -> None

// number <- (float / integer)
let (|Number|_|) = function
    | Rex rexFloat (x,r) -> numOf x r
    | Rex rexInt   (x,r) -> numOf x r
    | _ -> None                         

// array <- ( lb ( ({Nd}+) / identifier) rb ) array_part?
let (|ArrayIdx|_|) = function
    | '[' :: Int(i, ']' :: r) -> Some(i, r)
    | '[' :: Until [']' ] (x,r) -> match (toString x) with
                                   | "$idx" -> Some(-2, r)
                                   | "$len" -> Some(-1, r)
                                   | s  -> failwithf "not implemented: [%s]" s
    | _ -> None

// array_part <- ("." key)+ (array)?
let (|ArrayPart|_|) chars =
    let rec loop acc = function
        | '.' :: Key(k,rest) -> loop ( Name(toString(k)) :: acc) rest
        | rest when acc.Length > 0 -> match rest, acc with // optional index
                                      | ArrayIdx (i,r), tail -> Some(Index(i) :: tail, r)
                                      | _, acc               -> Some(acc, rest)
        | _ -> None
    loop [] chars

// (array / array_part)*
let (|ArrayOrPart|_|) chars =
    let rec loop acc = function
        | ArrayIdx  (i,r) -> loop ( Index(i) :: acc) r
        | ArrayPart (l,r) -> loop (l @ acc) r
        | rest when acc.Length > 0 -> Some(List.rev acc, rest)
        | _ -> None
    loop [] chars

// path <- key? ArrayOrPart+
//       / "." ArrayOrPart*
let (|IPath|_|) chars =
    let k,r = match chars with // optional key
                | Key(k,r) -> Some <| Name(toString(k)), r
                | _        -> None, chars

    let l,r = match r with
                | ArrayOrPart(a,r) -> a,r
                | '.' :: ArrayOrPart(a,r) when k = None -> a,r
                | _ -> [],r

    match k, l, chars.Head with
    | None,  [], '.' -> Some([FromCur], chars.Tail)
    | None,   _, '.' -> Some(FromCur :: l, r)
    | None,  [_], _  -> Some(l, r) // TODO check if needed, not test covered
    | Some(k), _, _  -> Some(k :: l,r)
    | _              -> None

// identifier <- path / key
let (|Ident|_|) = function
     | IPath(p,r) -> match p with
                     | [Name(n)] when n.Length > 1 -> Some(Key(n), r)
                     | _         -> Some(Path(p),r)
     | Key(k,r)  -> Some(Key(toString(k)),r) // not test covered
     | _ -> None

// param <- (key "=" (number / identifier / inline)
let (|Param|_|) = function
    | Key (k, '=' :: r) ->  let key = toString k
                            match r with
                            | Number (n,r) -> Some <| ((key, VNumber(n)), r)
                            | Int    (i,r) -> Some <| ((key, VNumber(decimal i)), r) // Int must follow Number in matching!
                            | Ident  (m,r) -> Some <| ((key, VIdent(m)), r)
                            | Inline (x,r) -> Some <| ((key, VInline(toString x)), r)
                            | _ -> None
    | _ -> None

// params <- (ws+ key "=" (number / identifier / inline) )*
let (|Params|_|) chars =
    let rec loop acc = function
        | Param(p,rest) ->  let rest2 = rest |> List.skipWhile Char.IsWhiteSpace
                            loop (p :: acc) (rest2)
        | rest when acc.Length > 0 -> Some(List.rev acc, rest)
        | _ -> None
    chars |> List.skipWhile Char.IsWhiteSpace |> loop []

// context <- (":" identifier)?
let (|Context|_|) = function
    | ':' :: Ident x -> Some x
    | _ -> None

let sectionType = function
    | '#' -> Scope
    | '?' -> Condition
    | '^' -> NotCondition
    | '<' -> Inline
    | '>'
    | '+' -> Block
    | '%' -> Escaped
    | '@' -> Helper
    | c -> failwithf "unknown tag %A" c

let folder (acc:Part list) (elem:Part) =
        match acc, elem with // replaces acc.Head when followed by elem
        | Buffer(a) :: tail, Special s -> elem :: Buffer(a.TrimEnd([|'\t'; '\n'; '\r'|])) :: tail
        | Special s :: _, Eol          -> acc // ignores Eol after special
        | Eol       :: _, Buffer(b)    -> Buffer(b.TrimStart([|' '; '\t'|])) :: acc
        | Buffer(a) :: tail, Buffer(b) -> let a2 = a.TrimEnd([|'\t'; '\n'; '\r'|]);
                                          if a2.Length < a.Length
                                          then Buffer(a2 + b.TrimStart([|' '; '\t'|])) :: tail
                                          else Buffer(a + b) :: tail
        | _                            -> elem :: acc

let compress body =
#if !WHITESPACE
    let tmp =
        body
        |> List.filter (fun p -> match p with | Comment(_) -> false | _ -> true )
        |> List.fold folder []

    match tmp with
    | Buffer(a) :: tail -> List.rev <| Buffer(a.TrimEnd([|'\t'; '\n'; '\r'|])) :: tail
    | _                 -> List.rev <| tmp
#else
    body
#endif

// partial <- ld (">"/"+") ws* (key / inline) context params ws* "/" rd
let (|PartialTag|_|) (r:char list) =
    let hd = r.Head
    match r with
    | '>' :: r
    | '+' :: r ->
        let r = r |> List.skipWhile Char.IsWhiteSpace
        let id,r =  match r with
                    | Key   (x,r) -> VIdent(Key(toString x)), r |> List.skipWhile Char.IsWhiteSpace
                    | Inline(x,r) -> VInline(toString x), r |> List.skipWhile Char.IsWhiteSpace
                    | _ -> failwith "expected (key / inline)"
        let ct,r =  match r with
                    | Context(m,r) -> Some m, r |> List.skipWhile Char.IsWhiteSpace
                    | _ -> None, r
        let pa,r2=  match r with
                    | Params(p,r) -> p, r |> List.skipWhile Char.IsWhiteSpace
                    | _ -> [], r |> List.skipWhile Char.IsWhiteSpace

        ////if ws <> [] then failwithf "unexpected whitespace before %s" (toString rest)
        match r2 with
        | '}' :: _ when hd = '>' -> failwith "expected self-closed tag"
        | '/' :: '}' :: r2 -> Some(Partial(id, ct, Map.ofList pa), r2)
                              // TODO start parsing the partial async  
        | _ -> None
    | _ -> None

//part <- raw / comment / section / partial / special / reference / buffer
let rec (|Part|_|) = function
    | '{' :: rest-> //// let ws,rest = rest |> collectws [] // ws needed for error handling below

                    // fails with CSS if Char.IsWhiteSpace rest.Head then failwithf "unexpected whitespace %s" (toString rest)
                    match rest with
                    | '!' :: Until ['!';'}'] (x,r) -> Some(Comment(toString x), r)
                    | '`' :: Until ['`';'}'] (x,r) -> Some(Buffer(toString x), r)
                    | PartialTag(p,r) -> Some(p,r)
                    // sec_tag_start <- ld [#?^<@%] ws* identifier context params
                    | '#' :: r | '?' :: r | '^' :: r | '<' :: r  | '@' :: r | '%' :: r | '+' :: r ->

                        let st = sectionType rest.Head
                        let r = r |> List.skipWhile Char.IsWhiteSpace
                        let id,r =  match r with
                                    | Ident(i,r) -> i, r |> List.skipWhile Char.IsWhiteSpace
                                    | _ -> failwith "expected (identifier)"
                        let ct,r =  match r with
                                    | Context (m,r) -> Some m, r |> List.skipWhile Char.IsWhiteSpace
                                    | _ -> None, r
                        let pa,r =  match r with
                                    | Params (p,r) -> p, r |> List.skipWhile Char.IsWhiteSpace
                                    | _ -> [], r
                        let pam = pa |> Map.ofList
                        // section <- sec_tag_start ws* rd body bodies end_tag / sec_tag_start ws* "/" rd

                        match r with
                        | Until ['}'] (x,r) -> // TODO maybe better just skip WS here
                            match x with // is tag closed?
                            | ['/'] ->  //// if ws <> [] then failwithf "unexpected whitespace before %s" (toString rest)
                                        Some(Section(st, id, ct, pam), r)
                            | '/' :: _ -> failwith "unexpected whitespace between / and }"
                            | _ ->
                                let rec loop acc ch =
                                    match ch with
                                    | []         -> ([], Null, [])
                                    | Part(p,r2) -> match p with
                                                    | End(i)       -> (acc, End(i), r2)
                                                    | NamedBody(k) -> (acc, NamedBody(k), r2)
                                                    | _            -> loop (p :: acc) r2
                                    | _          -> Buffer(toString ch) :: acc, Null, []

                                let bodyacc, endp, r = loop [] r
                                if endp = Null then 
                                    failwithf "missing end tag for %A" id
                                let body = bodyacc |> List.rev |> compress

                                let rec loop2 acc ch p =
                                    match p with
                                    | End(i)        ->  match id.ToPath(), i.ToPath() with
                                                        | s,e when s = e -> acc, ch
                                                        | FromCur :: s,e when s = e -> acc, ch
                                                        | _ -> failwithf "unexpected end {%s}...{/%s}" (id.ToString()) (i.ToString())                                                             
                                    | NamedBody(n)  ->  let b1, end2, r = loop [] r
                                                        let body = b1 |> List.rev |> compress
                                                        loop2 ((n, body) :: acc) r end2
                                    | _             ->  failwith "unexpected 1"
                                let bodies,r = (loop2 [] r endp)

                                //// if ws <> [] then failwithf "unexpected whitespace before %s" (toString rest)
                                match st with
                                | SectionType.Helper -> let name = id.ToString()
                                                        let logic = match name with
                                                                    | "eq" -> Eq | "ne" -> Ne | "lt" -> Lt | "lte" -> Lte | "gt" -> Gt | "gte" -> Gte
                                                                    | _ -> Custom
                                                        let part = if logic = Custom then Helper else LogicHelper(logic)
                                                        Some(SectionBlock(part, id, ct, pam, body, Map.ofList bodies), r)
                                | SectionType.Inline -> let key = id.ToString()
                                                        if key.Length > 0 && body.Length > 0 then
                                                            cache.[key] <- (DateTime.Now, body) // defines inline part TODO must not be cached globally!
                                                        Some(Comment(""), r) // returns nothing, cannot be NONE!
                                | _ ->                  Some(SectionBlock(st, id, ct, pam, body, Map.ofList bodies), r)
                        | _ -> failwith "syntax error"
                    // special <- ld "~" key rd
                    | '~' :: Until ['}'] (n,r) ->   let ch = match toString n with | "s"->' ' | "n"->'\n' | "r"->'\r' | "lb"->'{' | "rb"->'}' | _ -> failwith "unknown special"
                                                    Some(Special(ch), r)
                    // end_tag <- ld "/" ws* identifier ws* rd
                    | '/' :: Until ['}'] (x,r) ->
                        //// if ws <> [] then failwithf "unexpected whitespace before %s" (toString rest)

                        let chars = x |> List.skipWhile Char.IsWhiteSpace
                        match chars with
                        | Ident(i,_) -> Some(End(i), r)
                        | _ -> failwith "expected (identifier)"
                    // bodies <- (ld ":" key rd body)*
                    | ':' :: Key(k,Until ['}'] (_,r)) -> Some(NamedBody(toString k), r)
                    // reference <- ld identifier filters rd
                    | Ident(i, Until ['}'] (f,r)) -> let fs = (f |> toString).Split('|') |> parseFilters
                                                     Some(Reference(i, fs), r)

                    | _ ->  Some <| (Buffer "{", rest)
    // THE ORDERING OF THE FOLLOWING RULES IS IMPORTANT
    | '\r' :: '\n' :: chars
    | '\n' :: chars -> Some <| (Eol, chars)
    | BeforeEolOrTag (t,chars) ->  Some <| (Buffer(toString t), chars)
    | _ -> None

let parse text =
    let chars = text |> List.ofSeq
    let rec loop acc ch =
        match ch with
        | []           -> List.rev acc
        | Part(p,rest) -> loop (p :: acc) rest
        | x            -> List.rev (Buffer(toString x) :: acc)

    let body = loop [] chars
    compress <| body

// handles null obj and async results
let optional (o:obj) = 
    match o with
    | null               -> None
    | :? Async<'t> as a  -> a |> Async.RunSynchronously |> Some
    | o                  -> Some o

// extension method to use objects like a Map, works with ExpandoObjects
type System.Object with

  member o.TryFindProp (key:string) =
    match o with
    | null -> None
    | :? IDictionary<string,obj> as d ->    match d.ContainsKey key with
                                            | true  -> optional (d.Item(key))
                                            | _     -> None
    | :? IDictionary<string,Value> as d ->  match d.TryGetValue key with
                                            | true, v          -> Some (v :> obj)
                                            | _     -> None
    | _  ->                                 let p = o.GetType().GetProperty(key)
                                            match p with
                                            | null  -> None
                                            | _     -> optional (p.GetValue(o))
  member o.TryFindIndex (i:int) =
    match o with
    | :? IEnumerable<obj> as s ->
            match i with
            | -1 -> s |> Seq.cast<obj> |> Seq.last   |> Some
            | _  -> s |> Seq.cast<obj> |> Seq.skip i |> Seq.tryHead
    | _ -> failwith "object without index access"

open System.Runtime.CompilerServices
open Microsoft.FSharp.Reflection
open Microsoft.CSharp.RuntimeBinder

// read dynamic properties
// see https://gist.github.com/mattpodwysocki/300628
let (?) (o:obj) (name:string) : 'TargetResult  = 
    let targetResultType = typeof<'TargetResult>

    let toResult valu =
        try
                let res = Convert.ChangeType(valu, targetResultType);
                unbox(res)
        with
        | e->   printfn "%s: %A ? %s  %s" name valu (targetResultType.ToString()) e.Message
                Unchecked.defaultof<'TargetResult>                

    if not (FSharpType.IsFunction targetResultType)
    then 
        match o with
        | :? Params as p ->
            match p.TryFind name with
            | Some(v) -> match v with
                         | VIdent(_)  -> failwith "not supported" // c.Get i
                         | VNumber(n) -> toResult n
                         | VInline(s) -> toResult s
            | _       -> Unchecked.defaultof<'TargetResult>
       
        | :? IDictionary<string,obj> as dict ->
            match dict.TryGetValue name with
            | false,   _ -> failwith ("unknown property/key " + name)
            | true, valu -> match valu with
                            | :? 'TargetResult -> unbox(valu)
                            | null ->   Unchecked.defaultof<'TargetResult>
                            | _ ->      toResult valu       
        | _ ->  match o.TryFindProp name with
                | Some(v) -> toResult v
                | _ ->  let cs = CallSite<Func<CallSite, obj, obj>>.Create(Binder.GetMember(CSharpBinderFlags.None, name, null, [| CSharpArgumentInfo.Create(CSharpArgumentInfoFlags.None, null) |]))
                        unbox (cs.Target.Invoke(cs, o))
    else
        failwith "not implemented"

type Helper = Context -> BodyDict -> KeyValue -> (unit -> unit) -> unit
and Model =
    {
        Root:       obj
        ReadOnly:   bool
        Id:         string
        Path:       string
        LastWrite:  DateTime
    }
    static member empty = { Root = null; ReadOnly = true; Id = ""; Path = ""; LastWrite = DateTime.MinValue }

and Context =
    {   // Data context
        Parent:     Context option      // replaces Stack.Tail in orginal impl via chained Parents
        Current:    obj option          // replaces Stack.Head in orginal impl / "this" in data context
        Model:      Model
        Global:     obj                 // global data
        Index:      (int * int) option  // inside #array
        Culture:    CultureInfo         // used for formatting of values
        // execution data
        W:          TextWriter          // write used to render output
        TmplDir:    string              // the template source directory
        TmplName:   string
        Logger:     string -> unit      // a logger
        Helpers:    ConcurrentDictionary<string, Helper>
        // Options:    Whitespace
    }

    static member defaults = { TmplDir = ""; TmplName = ""; W = null; Global = null; Culture = CultureInfo.InvariantCulture;
                               Helpers = new ConcurrentDictionary<string, Helper>(); Model = Model.empty;
                               Current = None; Index = None; Parent = None; Logger = fun s -> (); }

    member this.ParseCachedAsync (parse:string -> Body) name = async {
        let path = Path.Combine(this.TmplDir, name)
        if not (File.Exists path) then
            this.Log <| sprintf "file not found: " + path
            return None
        else
            let date = File.GetLastWriteTime(path)
            
            let isValid name (lastWrite,_) =
                this.Log(System.String.Format("check {0} {1} <?= {2}", path, date, lastWrite))
                date <= lastWrite
            let update name = async {
                let sw = System.Diagnostics.Stopwatch()
                sw.Start()
                let body = File.ReadAllText path |> parse
                sw.Stop()
                this.Log(System.String.Format("parsed {0} {1:N3} [ms]", name, elapsedMs sw))
                return (date, body)
            }
            let! res = asyncMemoize isValid update name 
            return Some res
    }
    member this.ParseCached parse name : Body option =
        let res = this.ParseCachedAsync parse name |> Async.RunSynchronously
        match res with
        | Some (_, body) -> Some body
        | _              -> None

    member this.Write (v:Value)       = match v with
                                        | VNumber n -> n.ToString(this.Culture) |> this.Write
                                        | VInline s -> s |> this.Write
                                        | VIdent  i -> match this.Get i with | Some v -> this.WriteFiltered [] v | None -> ()
    member this.Write (s:string)      = this.W.Write(s)
    member this.Write (c:char)        = this.W.Write(c)
    // by default always apply the h filter, unless asked to unescape with |s
    member this.WriteFiltered (f:Filters) (v:obj) =
                                        let mutable o =
                                            match v with
                                            | :? decimal as d           ->  box <| d.ToString(this.Culture)
                                            | :? IEnumerable<obj> as ie ->  let arr = ie |> Seq.cast<obj> |> Seq.map( fun o -> Convert.ToString(o))
                                                                            String.Join(",", arr) :> obj
                                            | _ -> v

                                        if Seq.isEmpty f && o :? string then
                                            o <- System.Net.WebUtility.HtmlEncode(o :?> string) :> obj
                                        else for x in f do o <- x o
                                        this.W.Write(o)
    // write with interpolation of {data}
    member this.RexInterpolate (rex:Regex) (s:string)= 
                                        rex.Replace(s, (fun m ->    let tag = m.ToString();
                                                                    match this.Get (Key(tag.Substring(1, tag.Length-2))) with
                                                                    | Some(v) -> v.ToString()
                                                                    | None    -> tag
                                                   )   )    
    member this.WriteI (s:string)     = s |> this.RexInterpolate rexRefs |> this.W.Write
    member this.WriteI2(s:string)     = s |> this.RexInterpolate rexRefB |> this.W.Write
    member this.GetStr(key:string)    = match this.Get (Key key) with
                                        | Some(o) -> o.ToString()
                                        | None    -> ""
    member this.Log msg               = this.Logger msg

    member c.GetPartial name =          match cache.TryGetValue name with
                                        | true, (_, part) -> Some part
                                        | _ -> c.ParseCached parse name

    member c.TryFindSegment o = function
        | Name(n)  -> match o.TryFindProp n with
                      | Some(o) ->  match o with
                                    | :? Value as v -> match v with
                                                       | VIdent(i) -> c.Get i
                                                       | VNumber(n) -> Some(box n)
                                                       | VInline(s) -> Some(s |> c.RexInterpolate rexRefs |> box)
                                    | _ -> Some(o)
                      | _ -> None
        | Index(i) -> if i = -2 then o.TryFindIndex (fst c.Index.Value)
                      else o.TryFindIndex i
        | _ -> failwith "FromCur unexpected here"

    member c.Get (id:Identifier) : obj Option =
        let cur, path = match id with
                        | Identifier.Key(k)  -> let cur = k.StartsWith "."
                                                let k2 = if cur then k.Substring 1 else k
                                                if k2.Contains "." then (cur, k2.Split('.') |> List.ofArray |> List.map (fun x -> Name(x)) )
                                                                   else (cur, [Name(k2)])
                        | Identifier.Path(p) -> match p with
                                                | FromCur :: p -> true, p
                                                | _            -> false, p
        c.GetPath cur path

    member c.GetPath (cur:bool) (p:Segment list) : obj Option =
        let rec searchUpStack ctx =
            let value = match ctx.Current with | Some obj -> c.TryFindSegment obj p.Head | _ -> None
            match value, ctx.Parent with
            | None, Some(p) -> searchUpStack p
            | _             -> value

        let rec resolve o (path:Segment list) =
            match path.Tail, c.TryFindSegment o path.Head with
            | [], None -> None
            | _ , None -> None // failwithf "missing data %A" path // strict: indicates a path that cannot be resolved
            | [], x -> x
            | _ , Some o2 -> resolve o2 path.Tail

        match p with
        | [Name("$idx")] -> if c.Index.IsSome then Some(box <| fst c.Index.Value) else None
        | [Name("$len")] -> if c.Index.IsSome then Some(box <| snd c.Index.Value) else None
        | []             -> c.Current
        | _ ->
            let ctx,v = match cur with
                        | true  ->  c, None // fixed to current context
                        | false ->  let result = searchUpStack c // Search up the stack for the first value
                                    match result with // Try looking in the global context if we haven't found anything yet
                                    | Some _ -> c, result
                                    | None   -> c, (c.TryFindSegment c.Global p.Head)
            match v, p.Tail with
            | Some _ , [] -> v
            | Some o , _  -> resolve o p.Tail
            | None   , _  -> match ctx.Current with | Some o -> resolve o p | _ -> None

    member c.EvalBool cond =
        match c.Get cond with
        | None ->   false
        | Some o -> match o with
                    | null -> false
                    | :? bool as b -> b
                    | :? IEnumerable<obj> as ie ->  let en = ie.GetEnumerator()
                                                    en.MoveNext()
                    | _ ->  let s = o.ToString()
                            let result = s.Equals("true") || not (System.String.IsNullOrEmpty(s))
                            result

    member c.GetHelper name =
        let nullHelper (c:Context) (bodies:BodyDict) (param:KeyValue) (renderBody: unit -> unit) = ()

        match c.Helpers.TryGetValue name with
        | true, ref -> ref
        | _ ->  c.Log <| sprintf "missing helper: %s" name
                nullHelper // the nullHelper

    member c.RenderOpt (bo:Body Option) =
        match bo with
        | Some body -> c.Render body
        | None -> ()

    // render template parts with provided context & scope
    member c.Render (list:Part list) =
      for part in list do
        match part with
        //| Comment _      -> c.Write("<!-- " + text + " -->")
        | Special ch     -> c.Write(ch)
        | Buffer text    -> c.Write(text)
        | Partial(i,x,m) -> let n = match i with
                                    | VInline(i) -> i |> c.RexInterpolate rexRefs 
                                    | VIdent(i)  -> i.ToString()
                                    | _ -> failwith "unexpected"
                            if n <> "" then
                                    let cc = match x with // if a new context x is specified, then rebase WITHOUT parent
                                             | Some i -> { c with Parent = None;   TmplName = n; Current = c.Get i }
                                             | None   -> { c with Parent = Some c; TmplName = n; Current = Some(m :> obj) }
                                    match cache.TryGetValue n with
                                    | true, (_, part) -> Some part
                                    | _ -> c.ParseCached parse n
                                    |> cc.RenderOpt
        | Reference(k,f) -> match c.Get k with
                            | None -> ()
                            | Some value    ->  match value with
                                                | null -> ()
                                                | :? bool as b -> if b then c.WriteFiltered f value
                                                | _ ->  c.WriteFiltered f value
        | Section(st,n,_,pa) ->
                            match st with
                            | Scope  // scope without body could be a helper
                            | Helper         -> c.GetHelper (n.ToString()) c Map.empty pa (fun () -> c.Write "<!-- no body -->")
                            | LogicHelper(_) -> failwith "LogicHelper should be a SectionBlock"
                            | _ -> ()
        | SectionBlock(st,n,x,map,l,bodies) ->
            let renderIf (cc:Context) cond =   if cond then l |> cc.Render
                                               else bodies.TryFind "else" |> cc.RenderOpt
            match st with
            | Condition     ->  renderIf c (c.EvalBool n)
            | NotCondition  ->  renderIf c (not (c.EvalBool n))
            | Helper         -> c.GetHelper (n.ToString()) c bodies map (fun () -> l |> c.Render)
            | LogicHelper(t) -> let get s = match map.TryFind s with
                                            | Some(VInline(x)) -> box x
                                            | Some(VNumber(x)) -> box x
                                            | Some(VIdent(x))  -> match c.Get x with
                                                                    | Some(o) -> box o
                                                                    | None -> failwithf "%s must be defined" s
                                            | _ -> failwithf "missing %s" s

                                let l, rr = get "key", get "value"
                                let r = try Convert.ChangeType(rr, l.GetType()) // try align types to left side (data type)
                                        with | e -> rr
                                renderIf c   <| match t with
                                                | Eq -> l = r
                                                | Ne -> l <> r
                                                | Gt -> System.Convert.ToDouble(l) >  System.Convert.ToDouble(r)
                                                | Gte-> System.Convert.ToDouble(l) >= System.Convert.ToDouble(r)
                                                | Lt -> System.Convert.ToDouble(l) <  System.Convert.ToDouble(r)
                                                | Lte-> System.Convert.ToDouble(l) <= System.Convert.ToDouble(r)
                                                | _  -> false
            | Scope         ->  let cc =  match x with // if a new context x is specified, then rebase WITHOUT parent
                                                | Some i -> { c with Parent = None;   Current = c.Get i }
                                                | None   -> { c with Parent = Some c; Current = Some(map :> obj) }
                                match c.Get n with
                                | Some(valu) -> match valu with
                                                // Dust's default behavior is to enumerate over the array elem, passing each object in the array to the block.
                                                // When elem resolves to a value or object instead of an array, Dust sets the current context to the value
                                                // and renders the block one time.
                                                | :? IEnumerable<obj> as ie
                                                     -> let arr = ie |> Seq.cast<obj> |> Seq.toArray
                                                        let len = arr.Length
                                                        if len < 1 then renderIf cc false // = skip
                                                        else arr |> Array.iteri(fun i o -> l |> { cc with Parent = Some cc; Current = Some o; Index = Some(i,len) }.Render )
                                                | :? bool as b -> renderIf cc b
                                                | null -> renderIf cc false
                                                | o ->  l |> { cc with Parent = Some cc; Current = Some o }.Render
                                | None ->       match c.Helpers.TryGetValue (n.ToString()) with
                                                | true, ref -> ref cc Map.empty map (fun () -> failwith "not available")
                                                | _ ->         renderIf cc false
            | Block         ->  let name = n.ToString()
                                match cache.TryGetValue name with
                                | true, (_,b) -> b |> {c with TmplName = name}.Render // override
                                | _           -> l |> c.Render // default
            // Deprecated | Escaped       -> renderList (n :: scope) l
            | _ -> failwith <| sprintf "unexpected %A" st
    #if WHITESPACE
        | Eol -> c.Write '\n'
    #endif
        | _ -> ()

(* Dust Built-In Filters - Dust applies the h filter to all references by default,
   ensuring that variables are HTML-escaped. You can undo this autoescaping by appending the s filter.
h – HTML-encode
s – turn off automatic HTML encoding
j – Javascript string encode
u – encodeURI
uc – encodeURIComponent
js – JSON.stringify
jp – JSON.parse
*)
module DustFilters =
    let s (v:obj) : obj = v // System.Net.WebUtility.HtmlDecode(v :?> string) :> obj
    //let j (v:obj) : obj = System.Net.HttpUtility.JavaScriptStringEncode(v :?> string) :> obj
    let h (v:obj) : obj = System.Net.WebUtility.HtmlEncode(v :?> string) :> obj

filters.["s"] <- DustFilters.s
filters.["h"] <- DustFilters.h
