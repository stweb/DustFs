module Dust.Engine

open System.Collections.Generic
open System.Collections.Concurrent
open System.Text.RegularExpressions
open System.IO
open System

// key is defined as a character matching a to z, upper or lower case, followed by 0 or more alphanumeric characters
// key "key" = h:[a-zA-Z_$] t:[0-9a-zA-Z_$-]*
let _keyPattern  = "[a-zA-Z_\$][0-9a-zA-Z_\-\$]*" // h:[a-zA-Z_$] t:[0-9a-zA-Z_$-]*
let _keyPatternN = "(?<key>" + _keyPattern + ")" 

// path is defined as matching a key plus one or more characters of key preceded by a dot
// path "path" = k:key? d:(array_part / array)+
//             / "."    d:(array_part / array)*

// identifier is defined as matching a path or key
// identifier "identifier" = p:path / k:key
let _identPattern = "^\s*(?<ident>(\.|" + _keyPattern + ")" + "(\." + _keyPattern + ")*)" 

// context is defined as matching a colon followed by an identifier
// context = n:(":" n:identifier)?
let _ctxPattern = "^:" + _identPattern; // TODO

let _kvPattern = "(?<kvp>(" + _keyPatternN + "=(?<value>\".*?\"|\d+|[\.\w]+)" + ")\s*)*" 
let _keyValPattern = _identPattern +  
                     "\s*(?<ctx>" + _ctxPattern + ")??" +     
                     "\s*(?<all>" + _kvPattern + ")/{0,1}"

// reference is defined as matching a opening brace followed by an identifier plus one or more filters and a closing brace
// reference "reference" = { n:identifier f:filters }
// filters "filters" = f:("|" n:key )*

let rexRef = new Regex(_identPattern)
let rexKvp = new Regex(_keyValPattern)
let rexCtx = new Regex(_ctxPattern)
let rexStr = new Regex("\{[^}]*\}")
let rexStr2 = new Regex("\[(.*?)\]")

type Logic = 
    | Eq | Ne | Lt | Lte | Gt | Gte  // Logic Helper
//    | Sep | First | Last // Separator Helper
//    | Select | Any | None // Select Helper
//    | ContextDump // Debug Helper
    | Custom of string

type KeyValue = Map<string, string> 
type SectionType =
    | Scope // # - defines $idx and $len
    | Condition // ?name exists and evaluates to true
    | NotCondition // ^name doesn't exist or evaluates to false
    | Helper of Logic * KeyValue // @ logic helpers
    | Inline // < inline partials
    | Block // + Blocks in the base template can contain default content and a child template can override that content.
    | Escaped // % - deprecated?

type Filter = obj -> obj
type Filters = Filter seq

type Body = Part list
and Part =  // ld t:[#?^<+@%] n:identifier c:context p:params
    | Comment of string // "{! !}"  
    | Section of SectionType * string // self-closed element
    | SectionBlock of SectionType * string * Body  * BodyDict // block element
    | Partial of string * KeyValue // "{> (key/inline) context /}
    | Special of string // "{~ key}
    | Reference of string * Filters// {key[|filter1|filterN]}
    | Buffer of string
    | EndSection of string 
    | Bodies of string
and BodyDict = Map<string, Body> 

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

let optional o = if o = null then None else Some(o)

let elapsedMs (sw:System.Diagnostics.Stopwatch) =
    double sw.ElapsedTicks * 1000.0 / double System.Diagnostics.Stopwatch.Frequency

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

  member o.Get path = 
    match path with
    | [] -> None
    | h :: [] -> o.TryFindProp h
    | h :: tail ->  let xo = o.TryFindProp h
                    if xo.IsSome then xo.Value.Get tail else None

  member o.TryFind (key:string) = // TODO if key.StartsWith(".") then let cur = true key = key.substr(1)
    let path = key.Split('.') |> Array.toList;
    o.Get path

type Context = 
    {   _w:TextWriter; // private - access via this.Write
        _templateDir:string;
        data:obj;  
        index:int; // in #array 
        count:int; // of #array
        current:Option<obj>;
        scope:string list;
        logger: string -> unit
    }

    static member defaults = { _templateDir = ""; _w = null; data = null; index = 0; count = 0; current = None; scope = []; logger = fun s -> () }    

    member this.loadAndParse parse name = async {
        let sw = System.Diagnostics.Stopwatch()
        sw.Start()

        let fname = Path.Combine(this._templateDir, name )
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

    member this.parseCachedAsync parse = 
        this.loadAndParse parse |> asyncMemoize (fun name (lastWrite, _) -> 
                                                    let path = Path.Combine(this._templateDir, name )
                                                    let date = File.GetLastWriteTime(path)
                                                    this.Log(System.String.Format("check {0} {1} <?= {2}", path, date, lastWrite))
                                                    date <= lastWrite ) 

    member this.parseCached parse name = 
        let _, body = this.parseCachedAsync parse name |> Async.RunSynchronously
        body
    
    member this.Write (s:string)      = this._w.Write(s)
    member this.Write (c:char)        = this._w.Write(c)
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
                                        this._w.Write(o)
    member this.WriteSpecial(tag)     = this._w.Write(match tag with | "s"->' ' | "n"->'\n' | "r"->'\r' | "lb"->'{' | "rb"->'}' | _ -> failwith "not supported")
    member this.WriteI (s:string)     = this._w.Write(rexStr.Replace(s, (fun m ->   let tag = m.ToString();
                                                                                    let key = tag.Substring(1, tag.Length-2)
                                                                                    match this.Get(key) with
                                                                                    | Some(v) -> v.ToString()
                                                                                    | None    -> tag.ToUpper() )))
    member this.WriteI2(s:string)     = this._w.Write(rexStr2.Replace(s, (fun m ->  let tag = m.ToString();
                                                                                    let key = tag.Substring(1, tag.Length-2)
                                                                                    match this.Get(key) with
                                                                                    | Some(v) -> v.ToString()
                                                                                    | None    -> tag.ToUpper() )))                                                                                        
    member this.Get   (key:string)    = this.data.TryFind key
    member this.GetStr(key:string)    = match this.resolveRef this.scope key with
                                        | Some(o) -> o.ToString()
                                        | None    -> ""

    member this.Log msg = this.logger msg
    member this.Unresolved scope key =
#if DEBUG
                                        names.Add(key) |> ignore
                                        this.Log (sprintf "   ? %s in %A" key scope)
#endif  
                                        None
                                           
    member c.resolveRef scope (key:string) : Option<obj> =
        if key.StartsWith("\"") then             
            Some(key.Substring(1, key.Length-2) :> obj) // assumes proper double quoted
        elif key = "." then 
            c.current
        else
            let result = match (c.current) with
                         | Some obj -> obj.TryFind key
                         | None     -> None
            match result with 
            | Some _ -> result
            | None   -> let found = c.data.TryFind key
                        match found with                     
                        | Some _ -> found
                        | None   -> match scope with
                                    | []        ->  c.Unresolved scope key 
                                    | head::tail->  let key2 = head + "." + key
                                                    let result2 = c.data.TryFind key2 
                                                    match result2 with
                                                    | None ->   if tail = [] then c.Unresolved scope key 
                                                                else result2 // failwith "never?"
                                                    | _ -> result2

type Helper = Context -> BodyDict -> KeyValue -> (unit -> unit) -> unit        
let helpers = new ConcurrentDictionary<string, Helper>()

let nullHelper (c:Context) (bodies:BodyDict) (param:KeyValue) (renderBody: unit -> unit) =
    ()

helpers.[""] <- nullHelper

// --------------------------------------------------------------------------------------

let toString chars = System.String(chars |> Array.ofList)

let (|StartsWith|_|) prefix list = 
    let rec loop = function 
        | [], rest -> Some(rest)
        | p :: prefix, r :: rest when p = r -> loop (prefix, rest)
        | _ -> None
    loop (prefix, list)

let (|AsCharList|) (str : string) = str |> List.ofSeq

let rec parseDustTag closing acc = function 
    | StartsWith closing (rest) -> Some(List.rev acc, rest)
    | c :: chars -> parseDustTag closing (c :: acc) chars
    | _ -> None

let (|DustTag|_|) = function
    | '{' :: chars -> match chars with
                      | ' ' :: _ | '\n' :: _ | '\r' :: _ | '\t' :: _ -> None // TODO may be inaccurate
                      | '!' :: rest -> parseDustTag [ '!'; '}' ]  [chars.Head] rest
                      | _   :: rest -> parseDustTag [ '}' ]  [chars.Head] rest
                      | [] -> None
    | _ -> None


// --------------------------------------------------------------------------------------

// parse into map of key,value
let parseKeyValue input =
    let m = rexKvp.Match(input)
    if m.Success then
        let keys = m.Groups.["key"].Captures
        let ctx = m.Groups.["ctx"] // Some context
        let values = m.Groups.["value"].Captures
        ( 
            m.Groups.["ident"].Value, 
            (if ctx.Length > 0 then Some(ctx.Value) else None),
            [0 .. keys.Count-1] |> Seq.map (fun i -> (keys.[i].Value, values.[i].Value )) |> Map.ofSeq
        )
    else  
        let m = rexRef.Match(input)
        if m.Success then (m.Value, None, Map.empty)
        else failwith ("path expected " + input)

let parseSection ident param = function
    | '#' -> Scope
    | '?' -> Condition
    | '^' -> NotCondition
    | '<' -> Inline
    | '+' -> Block
    | '%' -> Escaped
    | '@' -> let logic = match ident with
                         | "eq" -> Eq | "ne" -> Ne | "lt" -> Lt | "lte" -> Lte | "gt" -> Gt | "gte" -> Gte
                         | _ -> Custom(ident)
             Helper(logic, param)
    | _ -> failwith "unknown tag" 

let parseFilters (strarr:seq<string>) =
    seq {
        for f in Seq.skip 1 strarr do
            let name = f.Trim()
            match filters.TryGetValue name with
            | true, filter -> yield filter
            | _ -> ()
    }

// tokenizes a stream of chars
let rec parseSpans (sec:Stack<string>) acc chars = 
    seq { 
        let emitLiteral() = seq { if acc <> [] then yield acc |> List.rev |> toString |> Buffer }
        match chars with
        | DustTag (inside, chars) -> 
            yield! emitLiteral ()
            if not inside.IsEmpty then
                let tag = (inside.Tail |> toString) // n:identifier c:context p:params TODO exact parsing               
                // printfn "parse %A" sec

                match inside.Head with 
                | '!' ->    yield Comment(inside |> toString)
                | ':' ->    yield Bodies(tag)
                | '~' ->    yield Special(tag) // k:key
                    // sec_tag_start is defined as matching an opening brace followed by one of #?^<+@% plus identifier plus context plus param
                    // followed by 0 or more white spaces
                    // sec_tag_start = ld t:[#?^<+@%] ws* n:identifier c:context p:params
                | '#' | '?' | '^' | '<' | '+' | '@' | '%'  ->             
                            let ident, ctx, kvp = parseKeyValue tag
                            let st = parseSection ident kvp inside.Head 
                        
                            if tag.EndsWith("/") then
                                match st with
                                | Helper(l,_) -> yield Section(Helper(l, kvp), ident)
                                | Block ->       yield Section(Block, ident)
                                | _ ->           failwith ("unexpected " + st.ToString())
                            else    
                                sec.Push ident 
                                yield SectionBlock(st, ident, [], Map.empty)

                | '/' ->    if tag <> sec.Peek() then 
                                failwith (sprintf "expected %s got %s" (sec.Peek()) tag)
                            sec.Pop() |> ignore
                            yield EndSection(tag)

                | '>' ->    let ident, ctx, kvp = parseKeyValue tag
                            yield Partial(ident, kvp)
                | _ ->      let s = toString inside
                            let m = rexRef.Match(s)
                            if m.Success then 
                                let filters = parseFilters (s.Split('|'))
                                yield Reference(m.Value, filters)
                            else 
                                yield Buffer("{" + toString(inside)  + "}")
            yield! parseSpans sec [] chars
        | c :: chars    ->  yield! parseSpans sec (c :: acc) chars
        | []            ->  yield! emitLiteral()
    }

// gather parts into sections and return the tree
let rec getTree acc stop = function
    | []                                        ->  (acc |> List.rev, []  , "")
    | Bodies(tag) :: tail                       ->  (acc |> List.rev, tail, tag)
    | EndSection(name) :: tail when name = stop ->  (acc |> List.rev, tail, "")
    | EndSection(name) :: _                     ->  failwith ("unexpected end " + name)
    | SectionBlock(typ,name,_,_) :: tail        ->  let body, rest, tag = getTree [] name tail
                                                    let (s,tail2) = if tag <> "" then
                                                                        let body2, rest2, _ = getTree [] name rest
                                                                        (SectionBlock(typ, name, body, [tag, body2] |> Map.ofList), rest2)
                                                                    else
                                                                        (SectionBlock(typ, name, body, Map.empty), rest)
                                                    if typ = Inline then
                                                        cache.[name] <- (DateTime.Now, body) // defines inline part, TODO local namespace
                                                        getTree (acc) stop tail2
                                                    else
                                                        getTree (s :: acc) stop tail2
    | head :: tail -> getTree (head :: acc) stop tail

let parse (doc:string) =
    let body,_,_ = doc |> List.ofSeq |> parseSpans (new Stack<string>()) [] |> Seq.toList |> getTree [] ""
    body 


// render template parts with provided context & scope
let rec render (c:Context) scope (part:Part) = 
    let renderList newscope parts = parts |> List.iter(fun p -> render c newscope p)

    let evalBool cond = match c.resolveRef scope cond with
                        | None ->   false // failwith ("undefined condition: " + cond)   
                        | Some o -> match o with
                                    | null -> false
                                    | :? bool as b -> b
                                    | :? IEnumerable<obj> as ie ->  let en = ie.GetEnumerator()
                                                                    en.MoveNext()
                                    | _ ->  let s = o.ToString()
                                            let result = s.Equals("true") || not (System.String.IsNullOrEmpty(s))
                                            result
    let helper name =
        match helpers.TryGetValue name with
        | true, ref -> ref
        | _ ->  c.Log <| sprintf "missing helper: %s" name
                helpers.[""] // the nullHelper                
                
    match part with
    //| Comment _      -> c.Write("<!-- " + text + " -->")
    | Special tag    -> c.WriteSpecial(tag)
    | Buffer text    -> if not (System.String.IsNullOrWhiteSpace(text)) then c.Write(text)
    | Partial(n, kv) -> let body = c.parseCached parse n
                        let c2 = { c with current = Some(kv :> obj) } 
                        // if not kv.IsEmpty then c.Log (">partial " + c2.current.Value.ToString())
                        if (body <> []) then body |> Seq.iter(fun p -> render c2 scope p)
    | Reference(k,f) -> match c.resolveRef scope k with                     
                        | None -> ()
                        | Some value    ->  match value with
                                            | null -> ()
                                            | :? bool as b -> if b then c.WriteFiltered f value
                                            | _ ->  c.WriteFiltered f value
    | Section(st, n) -> match st with 
                        | Block ->          match cache.TryGetValue n with
                                            | true, (_, part) -> part |> Seq.iter(fun p -> render c scope p) // else ignore
                                            | _ -> ()
                        | Scope ->          failwith "scope must have a body"
                        | Helper(l, map) -> match l with
                                            | Custom(name) ->   let c2 = { c with scope = scope }                        
                                                                helper name c2 Map.empty map (fun () -> ())
                                            | _ -> failwith "logic should be a SectionBlock" 
                        | _ -> ()
    | SectionBlock (st,n,l,bodies) -> 
        let renderIf cond = if cond then renderList (n :: scope) l 
                            else match bodies.TryFind "else" with
                                 | Some body -> renderList (n :: scope) body // TODO check if n should be added
                                 | None -> ()                            
        match st with 
        | Helper(t, map) -> match t with
                            | Custom(name)  -> helper name c bodies map (fun () -> renderList scope l)                                                             
                            | t ->  if not ((map.ContainsKey "key") && (map.ContainsKey "value")) then
                                        failwith "missing key/Value"
                            
                                    let mkey = map.["key"]
                                    let mval = map.["value"]
                                    if mkey = "type" then 
                                        printfn "%s" mval
                                    let ckey = c.resolveRef scope mkey
                                    let cval = c.resolveRef scope mval
                                    // TODO let ctyp = map.["type"] // e.g. "number"
                                    let comp = match ckey with                     
                                               | None    -> failwith "key must be defined" 
                                               | Some(l) -> match cval with
                                                            | None    -> false
                                                            | Some(r) -> match t with
                                                                            | Eq -> l.Equals(r)
                                                                            | Ne -> not (l.Equals(r))
                                                                            | Gt -> System.Convert.ToDouble(l) >  System.Convert.ToDouble(r)
                                                                            | Gte-> System.Convert.ToDouble(l) >= System.Convert.ToDouble(r)
                                                                            | Lt -> System.Convert.ToDouble(l) <  System.Convert.ToDouble(r)
                                                                            | Lte-> System.Convert.ToDouble(l) <= System.Convert.ToDouble(r)
                                                                            | _  -> failwith "TODO"
#if DEBUG2                                                   
                                    printfn "%A: %A %A %A %s %s" comp (match ckey with | Some(x) -> x.ToString() | _ -> "<n/a>") t (match cval with | Some(x) -> x.ToString() | _ -> "<n/a>") map.["key"] map.["value"] 
#endif
                                    renderIf comp                                                
        | Condition     ->  renderIf (evalBool n)
        | NotCondition  ->  renderIf (not (evalBool n))
        | Scope         ->  match c.resolveRef scope n with                     
                            | Some(valu) -> match valu with
                                            | :? IEnumerable<obj> as ie 
                                                 -> let arr = ie |> Seq.cast<obj> |> Seq.toArray
                                                    let len = arr.Length
                                                    if len < 1 then renderIf false
                                                    else arr |> Array.iteri(fun i o -> 
                                                        let c2 = { c with current = Some(o); index = i; count = len }
                                                        l |> List.iter(fun p -> render c2 (n :: scope) p) )
                                            | :? bool as b -> renderIf b
                                            | null -> renderIf false
                                            | o ->      let c2 = { c with current = Some(o); index = 0; count = 1 }
                                                        l |> List.iter(fun p -> render c2 (n :: scope) p) 
                            | None -> renderIf false      // TODO create new current from keyvalue map 
                                              
        | Block         -> // overrides base template
                           renderList (n :: scope) l 
                        
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
