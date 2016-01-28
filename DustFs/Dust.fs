module Dust.Engine

open System
open System.IO
open System.Collections.Generic
open System.Collections.Concurrent
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

//type Body = Part list
//and Part =  // ld t:[#?^<+@%] n:identifier c:context p:params
//    | Comment of string // "{! !}"  
//    | Section of SectionType * string // self-closed element
//    | SectionBlock of SectionType * string * Body  * BodyDict // block element
//    | Partial of string * string option * KeyValue // "{> (key/inline) context /}
//    | Special of string // "{~ key}
//    | Reference of string * Filters// {key[|filter1|filterN]}
//    | Buffer of string
//    | EndSection of string 
//    | Bodies of string
//    | Eol
//and BodyDict = Map<string, Body> 

type Body = Part list
and Part = 
    | Buffer of string
    | Comment of string
    | Partial of string * Identifier option * Params
    | Section of SectionType * Identifier * Identifier option * Params
    | SectionBlock of SectionType * Identifier * Identifier option * Params * Body * BodyDict
    | Special of string
    | Reference of Identifier * Filters
    | NamedBody of string
    | End of Identifier

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
    | _  ->     let t = o.GetType()
                let p = t.GetProperty(key)
                match p with
                | null -> None
                | _    -> optional (p.GetValue(o))

  member o.TryFindIndex (i:int) = 
    match o with
    | :? IEnumerable<obj> as s -> s |> Seq.cast<obj> |> Seq.skip i |> Seq.tryHead 
    | _ -> failwith "object without index access"

  member o.TryFindSegment = function
    | Index(i) -> o.TryFindIndex i
    | Name(n)  -> o.TryFindProp n 

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
    {   W:          TextWriter // private - access via this.Write
        TmplDir:    string
        Global:     obj // Global Context
        Index:      (int * int) option // in #array 
        Current:    obj option
        Parent:     Context option
        Logger:     string -> unit
    }
    override x.ToString() =
        match x.Index with
        | Some(ix,len) -> sprintf "%A %A %d/%d" x.Parent x.Current ix len
        | None         -> sprintf "%A %A"       x.Parent x.Current 

    static member defaults = { TmplDir = ""; W = null; Global = null; Current = None; Index = None; Parent = None; Logger = fun s -> () }    

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
                                            | _ -> v

                                        if Seq.isEmpty f && o :? string then
                                            o <- System.Net.WebUtility.HtmlEncode(o :?> string) :> obj 
                                        else for x in f do o <- x o                                        
                                        this.W.Write(o)
    member this.WriteSpecial(tag)     = this.W.Write(match tag with | "s"->' ' | "n"->'\n' | "r"->'\r' | "lb"->'{' | "rb"->'}' | _ -> failwith "not supported")
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
//    member this.GetStr(key:string)    = match this.resolveRef this.scope key with
//                                        | Some(o) -> o.ToString()
//                                        | None    -> ""

    member this.Log msg = this.Logger msg
    member this.Unresolved scope key =
#if DEBUG
                                        names.Add(key) |> ignore
                                        this.Log (sprintf "   ? %s in %A" key scope)
#endif  
                                        None

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
            let value = match ctx.Current with | Some obj -> obj.TryFindSegment p.Head | _ -> None
            match value, ctx.Parent with
            | None, Some(p) -> searchUpStack p
            | _             -> value

        let rec resolve o (path:Segment list) =
            match path.Tail, o.TryFindSegment path.Head with
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
                                        | None   -> c, (c.Global.TryFindSegment p.Head)
                        | _         ->  failwith "TODO ResolvePath ?"        
            match v, p.Tail with
            | Some _ , [] -> v
            | Some o , _  -> resolve o p.Tail
            | None   , _  -> match ctx.Current with | Some obj -> obj.TryFindSegment p.Head | _ -> None

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
    | '.' :: ArrayOrPart(a) ->  Some a
    | ArrayOrPart(a) ->  Some a
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
     | Path(p,r) -> Some(Path(p),r)
     | Key(k,r)  -> Some(Key(toString(k)),r)
     | _ -> None

// param <- (key "=" (number / identifier / inline)
let (|Param|_|) = function
    | Key (k, '=' :: r) ->  let key = toString k
                            match r with
                            | Inline (x,r) -> Some <| ((key, VInline(toString x)), r)
                            | Ident  (m,r) -> Some <| ((key, VIdent(m)), r)
                            | Int    (i,r) -> Some <| ((key, VNumber(decimal i)), r)
                            | Number (x,r) -> Some <| ((key, VNumber(Convert.ToDecimal(toString x))), r)
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

//part <- raw / comment / section / partial / special / reference / buffer
let rec (|Part|_|) = function
    | '{' :: rest-> match rest with
                    | '!' :: Until ['!';'}'] (x,r) -> Some(Comment(toString x), r)
                    | '`' :: Until ['`';'}'] (x,r) -> Some(Buffer(toString x), r)
                    // partial <- ld (">"/"+") ws* (key / inline) context params ws* "/" rd
                    | '>' :: Until ['/';'}'] (r,r2)
                    | '>' :: Until ['}'] (r,r2)
                    | '+' :: Until ['/';'}'] (r,r2) -> 
                        let st = sectionType rest.Head 
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
                            match x with // is tag closed?
                            | '/' :: _ -> Some(Section(st, id, ct, pam), r)
                            | _ -> 
                                let rec loop acc ch =  
                                    match ch with
                                    | Part(End(i),r)       -> (List.rev acc, End(i), r)
                                    | Part(NamedBody(k),r) -> (List.rev acc, NamedBody(k), r)                                                                
//                                    | StartsWith ['{';':'] r->  (List.rev acc, i, r)
                                    | Part(p,rest)      ->  loop (p :: acc) rest
                                    | x                 ->  printfn "BODY REST: %s | %s" (toString x) (toString r)
                                                            failwith "TODO"
//            | x                 ->  (List.rev (Buffer(toString x) ::acc), , r)

                                let body, endp, r = loop [] r

                                let rec loop2 acc ch p =  
                                    match p with
                                    | End(i) -> if i <> id then failwithf "unexpected enf  %A ... %A" id i else (acc, ch)
                                    | NamedBody(n)  ->  let body, end2, r = loop [] r
                                                        loop2 ((n, body) :: acc) r end2
                                    | _             ->  (acc,ch)
                                
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
                                                        cache.[key] <- (DateTime.Now, body) // defines inline part
                                                        Some(Comment(""), r) // returns nothing, cannot be NONE!
                                | _ ->                  Some(SectionBlock(st, id, ct, pam, body, Map.ofList bodies), r)
                        | _ -> failwith "syntax error"
                    // special <- ld "~" key rd
                    | '~' :: Until ['}'] (x,r) -> Some(Special(toString x), r)
                    // end_tag <- ld "/" ws* identifier ws* rd
                    | '/' :: Until ['}'] (x,r) -> 
                        let chars = x |> List.skipWhile Char.IsWhiteSpace 
                        match chars with
                        | Ident(i,_) -> Some(End(i), r)
                        | _ -> failwith "expected (identifier)"
                    // bodies <- (ld ":" key rd body)*
                    | ':' :: Key(k,Until ['}'] (_,r)) -> Some(NamedBody(toString k), r) 
                    // reference <- ld identifier filters rd
                    | Ident(i, Until ['}'] (_,r)) -> Some(Reference(i, []), r) // TODO filters

                    | _ -> None
    | Until [ '{' ] (t,chars) ->  Some <| (Buffer(toString t), '{' :: chars)
    | _ -> None

let parse text =
    let chars = text |> List.ofSeq
    let rec loop acc ch =  
        match ch with
        | Part(p,rest) -> loop (p :: acc) rest
        | [] -> List.rev acc
        | x -> List.rev (Buffer(toString x) :: acc)
    loop [] chars

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
    | Special tag    -> c.WriteSpecial(tag)
    | Buffer text    -> c.Write(text)
    | Partial(n,x,m) -> let body = match cache.TryGetValue n with
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
        let renderIf cond = if cond then renderList l 
                            else match bodies.TryFind "else" with
                                 | Some body -> renderList body // TODO check if n should be added
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
                            renderIf <| match t with
                                                                            | Eq -> l.Equals(r)
                                                                            | Ne -> not (l.Equals(r))
                                                                            | Gt -> System.Convert.ToDouble(l) >  System.Convert.ToDouble(r)
                                                                            | Gte-> System.Convert.ToDouble(l) >= System.Convert.ToDouble(r)
                                                                            | Lt -> System.Convert.ToDouble(l) <  System.Convert.ToDouble(r)
                                                                            | Lte-> System.Convert.ToDouble(l) <= System.Convert.ToDouble(r)
                                        | _  -> false
        | Condition     ->  renderIf (c.EvalBool n)
        | NotCondition  ->  renderIf (not (c.EvalBool n))
        | Scope         ->  match c.Get n with                     
                            | Some(valu) -> match valu with
                                            // Dust's default behavior is to enumerate over the array elem, passing each object in the array to the block.
                                            // When elem resolves to a value or object instead of an array, Dust sets the current context to the value
                                            // and renders the block one time.    
                                            | :? IEnumerable<obj> as ie 
                                                 -> let arr = ie |> Seq.cast<obj> |> Seq.toArray
                                                    let len = arr.Length
                                                    if len < 1 then renderIf false // = skip
                                                    else arr |> Array.iteri(fun i o -> 
                                                        let c2 = { c with Parent = Some c; Current = Some o; Index = Some(i,len) }
                                                        l |> List.iter(fun p -> render c2 p) )
                                            | :? bool as b -> renderIf b
                                            | null -> renderIf false
                                            | o ->      let c2 = { c with Parent = Some c; Current = Some o }
                                                        l |> List.iter(fun p -> render c2 p) 
                            | None -> renderIf false      // TODO create new current from keyvalue map     
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
