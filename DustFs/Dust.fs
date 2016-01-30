module Dust.Engine

open System
open System.IO
open System.Collections.Generic
open System.Collections.Concurrent
open System.Globalization
open System.Text.RegularExpressions

let rexStr = new Regex("\{[^}]*\}")
let rexStr2 = new Regex("\[(.*?)\]")
    
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
    | Name of string
    | Index of int
with 
    override x.ToString() = 
        match x with 
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
    | Partial of string * Identifier option * Params
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
let unquote (s:string) = if s.StartsWith("\"") then s.Substring(1, s.Length-2) else s

let inline optional o = if o = null then None else Some(o)

let elapsedMs (sw:System.Diagnostics.Stopwatch) =
    double sw.ElapsedTicks * 1000.0 / double System.Diagnostics.Stopwatch.Frequency

let inline toString chars = System.String(chars |> Array.ofList)

let (|Rex|_|) pattern chars =
    let input = chars |> toString
    let m = Regex.Match(input, pattern) 
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

let (|BeforeOneOf|_|) chars = // [ '\n', '{' ] 
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

let (|Whitespace|_|) ch = 
    match Char.IsWhiteSpace ch with
    | true -> Some(ch)
    | _ -> None


// extension method to use objects like a Map, works with ExpandoObjects
type System.Object with    // TODO if key.StartsWith(".") then let cur = true key = key.substr(1)

  member o.TryFindProp (key:string) = 
    match o with
    | null -> None
    | :? IDictionary<string,string> as d -> match d.ContainsKey key with
                                            | true -> optional (d.Item(key) |> unquote :> obj)
                                            | _ -> None
    | :? IDictionary<string,obj> as d ->    match d.ContainsKey key with
                                            | true -> optional (d.Item(key))
                                            | _ -> None
    | :? IDictionary<string,Value> as d ->  match d.TryGetValue key with
                                            | true, VInline(s) -> Some (s :> obj)
                                            | true, v          -> Some (v :> obj)
                                            | _ -> None
    | _  ->     let t = o.GetType()
                let p = t.GetProperty(key)
                match p with
                | null -> None
                | _    -> optional (p.GetValue(o))

  member o.TryFindIndex (i:int) = 
    match o with
    | :? IEnumerable<obj> as s -> s |> Seq.cast<obj> |> Seq.skip i |> Seq.tryHead 
    | _ -> failwith "object without index access"

//  member o.Get path = 
//    match path with
//    | [] -> None
//    | [h] -> o.TryFindProp h
//    | h :: tail ->  let xo = o.TryFindProp h
//                    if xo.IsSome then xo.Value.Get tail else None
//
//  member o.TryFind (key:string) = // TODO if key.StartsWith(".") then let cur = true key = key.substr(1)
//    let path = key.Split('.') |> Array.toList;
//    o.Get path

type Context = 
    {   
        // Data context
        Parent:     Context option      // replaces Stack.Tail in orginal impl via chained Parents
        Current:    obj option          // replaces Stack.Head in orginal impl / "this" in data context
        Global:     obj                 // global data
        Index:      (int * int) option  // inside #array 
        Culture:    CultureInfo         // used for formatting of values
        // execution data
        W:          TextWriter          // write used to render output
        TmplDir:    string              // the template source directory
        Logger:     string -> unit      // a logger
        // Options:    Whitespace
    }
    override x.ToString() =
        match x.Index with
        | Some(ix,len) -> sprintf "%A %A %d/%d" x.Parent x.Current ix len
        | None         -> sprintf "%A %A"       x.Parent x.Current 

    static member defaults = { TmplDir = ""; W = null; Global = null; Culture = CultureInfo.InvariantCulture;
                               Current = None; Index = None; Parent = None; Logger = fun s -> () }    

    member this.LoadAndParse parse name = async {
        let sw = System.Diagnostics.Stopwatch()
        sw.Start()

        let fname = Path.Combine(this.TmplDir, name )
        let writeTime = File.GetLastWriteTime(fname)
        let body =  
            if File.Exists fname then 
                File.ReadAllText fname |> parse
            else
                this.Log <| sprintf "file not found: " + fname      
                []

        sw.Stop()
        this.Log(System.String.Format("parsed {0} {1:N3} [ms]", name, elapsedMs sw))
        return (writeTime, body)
    }

    member this.ParseCachedAsync parse = 
        this.LoadAndParse parse |> asyncMemoize (fun name (lastWrite, _) -> 
                                                    let path = Path.Combine(this.TmplDir, name )
                                                    let date = File.GetLastWriteTime(path)
                                                    this.Log(System.String.Format("check {0} {1} <?= {2}", path, date, lastWrite))
                                                    date <= lastWrite ) 
    member this.ParseCached parse name = 
        let _, body = this.ParseCachedAsync parse name |> Async.RunSynchronously
        body
    
    member this.Write (s:string)      = this.W.Write(s)
    member this.Write (c:char)        = this.W.Write(c)
    // by default always apply the h filter, unless asked to unescape with |s
    member this.WriteFiltered (f:Filters) (v:obj) = 
                                        let mutable o = 
                                            match v with
                                            | :? IEnumerable<obj> as ie ->  let arr = ie |> Seq.cast<obj> |> Seq.map( fun o -> Convert.ToString(o))
                                                                            String.Join(",", arr) :> obj
                                            | :? decimal as d -> box <| d.ToString(this.Culture)
                                            | _ -> v

                                        if Seq.isEmpty f && o :? string then
                                            o <- System.Net.WebUtility.HtmlEncode(o :?> string) :> obj 
                                        else for x in f do o <- x o                                        
                                        this.W.Write(o)
//    member this.WriteI (s:string)     = this.W.Write(rexStr.Replace(s, (fun m ->    let tag = m.ToString();
//                                                                                    let key = tag.Substring(1, tag.Length-2)
//                                                                                    match this.GetKey(key) with
//                                                                                    | Some(v) -> v.ToString()
//                                                                                    | None    -> tag.ToUpper() )))
//    member this.WriteI2(s:string)     = this.W.Write(rexStr2.Replace(s, (fun m ->   let tag = m.ToString();
//                                                                                    let key = tag.Substring(1, tag.Length-2)
//                                                                                    match this.GetKey(key) with
//                                                                                    | Some(v) -> v.ToString()
//                                                                                    | None    -> tag.ToUpper() )))                                                                                        
//    member this.GetKey(key:string)    = this.Data.TryFind key
    member this.GetStr(key:string)    = match this.Get (Key key) with
                                        | Some(o) -> o.ToString()
                                        | None    -> ""

    member this.Log msg = this.Logger msg
    member this.Unresolved scope key =
#if DEBUG
                                        names.Add(key) |> ignore
                                        this.Log (sprintf "   ? %s in %A" key scope)
#endif  
                                        None

      member c.TryFindSegment o = function
        | Name(n)  -> match o.TryFindProp n with
                      | Some(o) ->  match o with
                                    | :? Value as v -> match v with 
                                                       | VIdent(i) -> c.Get i
                                                       | VNumber(n) -> Some(box n)
                                                       | _ -> failwith "unexpected"
                                    | _ -> Some(o)
                      | _ -> None
        | Index(i) -> o.TryFindIndex i

    member c.Get (id:Identifier) : obj Option =      
        let cur, path = match id with
                        | Key(k)  -> let cur = k.StartsWith "." 
                                     let k2 = if cur then k.Substring 1 else k
                                     if k2.Contains "." then failwith "TODO split at dot"
                                     (cur, [Name(k2)])
                        | Path(p) -> false, p
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
            | _ , None -> failwith "bad path"
            | [], x -> x
            | _ , Some o2 -> resolve o2 path.Tail

        match p with
        | [Name("$idx")] -> if c.Index.IsSome then Some(box <| fst c.Index.Value) else None
        | [Name("$len")] -> if c.Index.IsSome then Some(box <| snd c.Index.Value) else None
        | [Name(".")]    -> c.Current
        | _ ->
            let ctx,v = match cur, p with
                        | true, [_] ->  c, None // fixed to current context
                        | false, _  ->  let result = searchUpStack c // Search up the stack for the first value
                                        match result with // Try looking in the global context if we haven't found anything yet                                       
                                        | Some _ -> c, result
                                        | None   -> c, (c.TryFindSegment c.Global p.Head)
                        | _         ->  failwith "TODO ResolvePath ?"        
            match v, p.Tail with
            | Some _ , [] -> v
            | Some o , _  -> resolve o p.Tail
            | None   , _  -> match ctx.Current with | Some obj -> c.TryFindSegment obj p.Head | _ -> None

    member c.EvalBool cond = 
        match c.Get cond with
        | None ->   false // failwith ("undefined condition: " + cond)   
        | Some o -> match o with
                    | null -> false
                    | :? bool as b -> b
                    | :? IEnumerable<obj> as ie ->  let en = ie.GetEnumerator()
                                                    en.MoveNext()
                    | _ ->  let s = o.ToString()
                            let result = s.Equals("true") || not (System.String.IsNullOrEmpty(s))
                            result

type Helper = Context -> BodyDict -> KeyValue -> (unit -> unit) -> unit        
let helpers = new ConcurrentDictionary<string, Helper>()

let nullHelper (c:Context) (bodies:BodyDict) (param:KeyValue) (renderBody: unit -> unit) =
    ()

helpers.[""] <- nullHelper

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
    | Quoted(x) -> Some x // TODO
    | _         -> None    

// key <- (({Ll} / {Lu} / [_$]) ({Nd} / {Ll} / {Lu} / [_$-])*)
// IMPORTANT RegEx must start with '^' to match from the start
let (|Key|_|) = function
    | Rex "^[a-zA-Z_\$][0-9a-zA-Z_\-\$]*" x -> Some x
    | _ -> None

let (|Int|_|) = function
    | Rex "^[0-9]+" (i,r) -> Some(Convert.ToInt32(toString i),r)
    | _ -> None

// number <- (float / integer)
let (|Number|_|) = function
    | Rex "^(-)?[0-9]*(?:\.[0-9]*)?" x -> Some x
    | Rex "^[0-9]+" x -> Some x
    | _ -> None

// array <- ( lb ( ({Nd}+) / identifier) rb ) array_part?
let (|ArrayIdx|_|) = function
    | '[' :: Int(i, ']' :: r) -> Some(i, r)
    // | '[' :: Ident(i, ']' :: r) -> failwith "TODO"
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

// path <- "." ArrayOrPart* / key? ArrayOrPart+ 
let (|Path|_|) = function    
    | '.' :: ArrayOrPart(p,r) ->    match p with
                                    | [Name(n)] -> Some([Name("." + n)], r)
                                    | _         -> Some(p,r)
    | ArrayOrPart(a)        ->  Some a
    | '.' :: r              ->  Some([Name(".")], r)
    | chars                 ->  let k,r = match chars with // optional key
                                          | Key(k,r) -> Some <| Name(toString(k)), r
                                          | _        -> None, chars
                                match r with
                                | ArrayOrPart(a,r) -> match k with 
                                                      | Some(k) -> Some(k :: a,r)
                                                      | _       -> Some(a,r)
                                | _                -> None
// identifier <- path / key
let (|Ident|_|) = function
     | Path(p,r) -> match p with 
                    | [Name(n)] when n.Length > 1 -> Some(Key(n), r) 
                    | _         -> Some(Path(p),r)
     | Key(k,r)  -> Some(Key(toString(k)),r)
     | _ -> None

// param <- (key "=" (number / identifier / inline)
let (|Param|_|) = function
    | Key (k, '=' :: r) ->  let key = toString k
                            match r with
                            | Inline (x,r) -> Some <| ((key, VInline(toString x)), r)
                            | Ident  (m,r) -> Some <| ((key, VIdent(m)), r)
                            | Int    (i,r) -> Some <| ((key, VNumber(decimal i)), r)
                            | Number (x,r) -> match Decimal.TryParse(toString x, NumberStyles.Any, CultureInfo.InvariantCulture) with
                                              | true, n -> Some <| ((key, VNumber(n)), r)
                                              | _       -> failwith "bad number"
                            | _ -> None
    | _ -> None    

// params <- (ws+ key "=" (number / identifier / inline) )*
let (|Params|_|) chars = 
    let ch1 = chars |> List.skipWhile Char.IsWhiteSpace 
    let rec loop acc = function 
        | Param(p,rest) -> loop (p :: acc) (rest |> List.skipWhile Char.IsWhiteSpace)
        | rest when acc.Length > 0 -> Some(List.rev acc, rest)
        | _ -> None
    loop [] ch1

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
        //| Buffer(a) :: tail, Eol       -> Buffer(a+"\n") :: tail
        | Buffer(a) :: tail, Special s -> elem :: Buffer(a.TrimEnd([|'\t'; '\n'; '\r'|])) :: tail
        | Special s :: _, Eol          -> acc // ignores Eol after special
        | Eol       :: _, Buffer(b)    -> Buffer(b.TrimStart([|' '; '\t'|])) :: acc
        | Buffer(a) :: tail, Buffer(b) -> //let b1 = b.TrimStart([|' '; '\t'|]) // ensure one space is kept, TODO verify
                                          //let b2 = if b1.Length = 0 then " " else b1
                                          let a2 = a.TrimEnd([|'\t'; '\n'; '\r'|]);
                                          if a2.Length < a.Length
                                          then Buffer(a2 + b.TrimStart([|' '; '\t'|])) :: tail
                                          else Buffer(a + b) :: tail
        | _                            -> elem :: acc

let compress body =
    let tmp = 
        body
        |> List.filter (fun p -> match p with | Comment(_) -> false | _ -> true )
        |> List.fold folder [] 

    match tmp with
    | Buffer(a) :: tail -> List.rev <| Buffer(a.TrimEnd([|'\t'; '\n'; '\r'|])) :: tail
    | _                 -> List.rev <| tmp

//part <- raw / comment / section / partial / special / reference / buffer
let rec (|Part|_|) = function
    | '{' :: rest-> match rest with
                    | '!' :: Until ['!';'}'] (x,r) -> Some(Comment(toString x), r)
                    | '`' :: Until ['`';'}'] (x,r) -> Some(Buffer(toString x), r)
                    // partial <- ld (">"/"+") ws* (key / inline) context params ws* "/" rd
                    | '>' :: Until ['/';'}'] (r,r2)
                    | '>' :: Until ['}'] (r,r2)
                    | '+' :: Until ['/';'}'] (r,r2) -> 
                        //let st = sectionType rest.Head 
                        let r = r |> List.skipWhile Char.IsWhiteSpace 
                        let id,r =  match r with
                                    | Key   (x,r) -> toString x, r |> List.skipWhile Char.IsWhiteSpace 
                                    | Inline(x,r) -> toString x, r |> List.skipWhile Char.IsWhiteSpace 
                                    | _ -> failwith "expected (key / inline)"              
                        let ct,r =  match r with
                                    | Context(m,r) -> Some m, r |> List.skipWhile Char.IsWhiteSpace 
                                    | _ -> None, r
                        let pa,r =  match r with
                                    | Params(p,r) -> p, r |> List.skipWhile Char.IsWhiteSpace 
                                    | _ -> [], r

                        Some(Partial(id, ct, Map.ofList pa), r2)
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
                        | Until ['}'] (x,r) ->  
                            // printfn "SECTION %A %s %A until %A | %A" (rest.Head) (id.ToString()) rrr x (r |> List.take 3)
                            match x with // is tag closed?
                            | '/' :: _ -> Some(Section(st, id, ct, pam), r)
                            | _ -> 
                                let rec loop acc ch =  
                                    match ch with
                                    | []         -> ([], Null, [])
                                    | Part(p,r2) -> match p with
                                                    | End(i)       -> (acc, End(i), r2)
                                                    | NamedBody(k) -> (acc, NamedBody(k), r2)
                                                    | _            -> loop (p :: acc) r2
                                    | _          -> failwith "unexpected"

                                let bodyacc, endp, r = loop [] r
                                let body = bodyacc |> List.rev |> compress
                                
                                let rec loop2 acc ch p =  
                                    match p with
                                    | End(i) -> if i <> id then failwithf "unexpected end %A ... %A" id i else (acc, ch)
                                    | NamedBody(n)  ->  let b1, end2, r = loop [] r
                                                        let body = b1 |> List.rev |> compress
                                                        loop2 ((n, body) :: acc) r end2
                                    | Null          ->  ([], [])
                                    | _             ->  failwith "really?" //(acc,ch)
                                
                                let bodies,r = (loop2 [] r endp)        
                                
                                match st with
                                | SectionType.Helper -> let name = id.ToString()
                                                        let logic = match name with
                                                                    | "eq" -> Eq | "ne" -> Ne | "lt" -> Lt | "lte" -> Lte | "gt" -> Gt | "gte" -> Gte
                                                                    | _ -> Custom
                                                        if logic = Custom then
                                                            Some(Section(Helper, id, ct, pam), r)
                                                        else                                                        
                                                            Some(Section(LogicHelper(logic), id, ct, pam), r)
                                | SectionType.Inline -> let key = id.ToString()
                                                        if key.Length > 0 && body.Length > 0 then
                                                            cache.[key] <- (DateTime.Now, body) // defines inline part
                                                        Some(Comment(""), r) // returns nothing, cannot be NONE!
                                | _ ->                  Some(SectionBlock(st, id, ct, pam, body, Map.ofList bodies), r)
                        | _ -> failwith "syntax error"
                    // special <- ld "~" key rd
                    | '~' :: Until ['}'] (n,r) ->   let ch = match toString n with | "s"->' ' | "n"->'\n' | "r"->'\r' | "lb"->'{' | "rb"->'}' | _ -> failwith "unknown special"
                                                    Some(Special(ch), r)
                    // end_tag <- ld "/" ws* identifier ws* rd
                    | '/' :: Until ['}'] (x,r) -> 
                        let chars = x |> List.skipWhile Char.IsWhiteSpace 
                        match chars with
                        | Ident(i,_) -> Some(End(i), r)
                        | _ -> failwith "expected (identifier)"
                    // bodies <- (ld ":" key rd body)*
                    | ':' :: Key(k,Until ['}'] (_,r)) -> Some(NamedBody(toString k), r) 
                    // reference <- ld identifier filters rd
                    | Ident(i, Until ['}'] (f,r)) -> let fs = (f |> toString).Split('|') |> parseFilters 
                                                     Some(Reference(i, fs), r) // TODO filters

                    | _ -> None
    // THE ORDERING OF THE FOLLOWING RULES IS IMPORTANT
    | '\r' :: '\n' :: chars 
    | '\n' :: chars -> Some <| (Eol, chars)
    | BeforeOneOf (t,chars) ->  Some <| (Buffer(toString t), chars)
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

// render template parts with provided context & scope
let rec render (c:Context) (part:Part) = 
    let inline renderList parts = parts |> List.iter(fun p -> render c p)   

    let helper name =
        match helpers.TryGetValue name with
        | true, ref -> ref
        | _ ->  c.Log <| sprintf "missing helper: %s" name
                helpers.[""] // the nullHelper                

                
    match part with
    //| Comment _      -> c.Write("<!-- " + text + " -->")
    | Special ch     -> c.Write(ch)
    | Buffer text    -> c.Write(text)
    | Partial(k,x,m) -> let n = if k = "{name}" then c.GetStr "name" else k   
                        let body = match cache.TryGetValue n with
                                   | true, (_, part) -> part 
                                   | _ -> c.ParseCached parse n
                        let c2 = { c with Parent = Some c
                                          Current = match x with
                                                    | Some i -> c.Get i
                                                    | None -> Some(m :> obj)
                                 } 
                        body |> Seq.iter(fun p -> render c2 p)
    | Reference(k,f) -> match c.Get k with                     
                        | None -> ()
                        | Some value    ->  match value with
                                            | null -> ()
                                            | :? bool as b -> if b then c.WriteFiltered f value                                            
                                            | _ ->  c.WriteFiltered f value
    | Section(st,n,_,pa) -> 
                        match st with 
//                        | Block ->          match cache.TryGetValue n with 
//                                            | true, (_, part) -> part |> Seq.iter(fun p -> render c p) // else ignore
//                                            | _ -> ()
                        | Scope ->          failwith "scope must have a body"
                        | Helper         -> helper (n.ToString()) c Map.empty pa (fun () -> failwith "not available")
                        | LogicHelper(_) -> failwith "LogicHelper should be a SectionBlock" 
                        | _ -> ()
    | SectionBlock(st,n,_,map,l,bodies) -> 
        let renderIf cc cond = if cond then l |> List.iter(fun p -> render cc p) 
                               else match bodies.TryFind "else" with
                                    | Some body -> body |> List.iter(fun p -> render cc p) 
                                    | None -> ()                            
        match st with 
        | Helper         -> helper (n.ToString()) c bodies map (fun () -> renderList l)
        | LogicHelper(t) -> let get s = match map.TryFind s with
                                        | Some(VInline(x)) -> box x
                                        | Some(VNumber(x)) -> box x 
                                        | Some(VIdent(x))  -> match c.Get x with
                                                                | Some(o) -> box o
                                                                | None -> failwithf "%s must be defined" s
                                        | _ -> failwithf "missing %s" s

                            let l, r = get "key", get "value"
                            renderIf c <| match t with
                                        | Eq -> l.Equals(r)
                                        | Ne -> not (l.Equals(r))
                                        | Gt -> System.Convert.ToDouble(l) >  System.Convert.ToDouble(r)
                                        | Gte-> System.Convert.ToDouble(l) >= System.Convert.ToDouble(r)
                                        | Lt -> System.Convert.ToDouble(l) <  System.Convert.ToDouble(r)
                                        | Lte-> System.Convert.ToDouble(l) <= System.Convert.ToDouble(r)
                                        | _  -> false
        | Condition     ->  renderIf c (c.EvalBool n)
        | NotCondition  ->  renderIf c (not (c.EvalBool n))
        | Scope         ->  let cc = if map.IsEmpty then c else { c with Parent = Some c; Current = Some (map :> obj) }
                            match c.Get n with                     
                            | Some(valu) -> match valu with
                                            // Dust's default behavior is to enumerate over the array elem, passing each object in the array to the block.
                                            // When elem resolves to a value or object instead of an array, Dust sets the current context to the value
                                            // and renders the block one time.    
                                            | :? IEnumerable<obj> as ie 
                                                 -> let arr = ie |> Seq.cast<obj> |> Seq.toArray
                                                    let len = arr.Length
                                                    if len < 1 then renderIf cc false // = skip
                                                    else arr |> Array.iteri(fun i o -> 
                                                        let c2 = { cc with Parent = Some cc; Current = Some o; Index = Some(i,len) }
                                                        l |> List.iter(fun p -> render c2 p) )
                                            | :? bool as b -> renderIf cc b
                                            | null -> renderIf cc false
                                            | o ->      let c2 = { cc with Parent = Some cc; Current = Some o }
                                                        l |> List.iter(fun p -> render c2 p) 
                            | None -> renderIf cc false      // TODO create new current from keyvalue map     
        | Block         -> match cache.TryGetValue (n.ToString()) with 
                           | true, (_,b) -> b |> Seq.iter(fun p -> render c p) // override
                           | _           -> renderList l // default                        
        // Deprecated | Escaped       -> renderList (n :: scope) l  
        | _ -> failwith <| sprintf "unexpected %A" st
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
