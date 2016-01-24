module Dust.Test
 
open NUnit.Framework
open FsUnit
open Dust.Engine
open System
open System.IO
open System.Collections.Generic
open System.Dynamic
open Newtonsoft.Json
open Newtonsoft.Json.Converters

// test DSL

let empty = 
    new ExpandoObject()

let js code =
    empty // TODO

let json s =
    try
        JsonConvert.DeserializeObject<ExpandoObject>(s, new ExpandoObjectConverter()) :> obj;
    with
    | ex -> printfn "EXCEPTION %s in %s" ex.Message s
            ex.Message  :> obj;  

let dustExec name body data =
    let sb = System.Text.StringBuilder()
    let ctx = { Context.defaults with W = new StringWriter(sb); TmplDir = __SOURCE_DIRECTORY__ + """\null\""" ; Data = data; Current = Some(data)}
    body |> List.iter(fun p -> render ctx [] p)
    sb.ToString() //.Replace("\r\n", "\n")

let dust name source data =
    dustExec name (parse source) data    

let dustReg name source data =
    let body = parse source
    cache.[name] <- (DateTime.Now, body)
    dustExec name body data    

let shouldEqual (x: 'a) (y: 'a) = 
    Assert.AreEqual(x, y, sprintf "Expected: %A\nActual: %A" x y)

let shouldBeSome (x: string) (y: obj option) = 
    Assert.AreEqual(x, y.Value :?> string, sprintf "Expected: %A\nActual: %A" x y)

let shouldBeSomeS (x: string) (y: string option) = 
    Assert.AreEqual(x, y.Value, sprintf "Expected: %A\nActual: %A" x y)

let save out exp =
#if DEBUG
    File.WriteAllText(@"d:\out.txt", out)
    File.WriteAllText(@"d:\exp.txt", exp)
#endif
    ()

let expect a b =
    if not (a=b) then save a b
    shouldEqual a b