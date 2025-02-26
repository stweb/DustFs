module Dust.Test
 
open FsUnit
open Dust.Engine
open System
open System.IO
open System.Collections.Concurrent
open System.Dynamic
open Newtonsoft.Json
open Newtonsoft.Json.Converters

// test DSL

let helpers = new ConcurrentDictionary<string, Helper>()

let empty = 
    new ExpandoObject()

let json s =
    JsonConvert.DeserializeObject<ExpandoObject>(s, new ExpandoObjectConverter()) :> obj;

let dustExec body name data glob =    
    let sb = System.Text.StringBuilder()
    let ctx = { Context.defaults with 
                    W = new StringWriter(sb)
                    TmplDir = __SOURCE_DIRECTORY__ + """\null\"""
                    TmplName = name
                    Current = Some data
                    Global = glob
                    Helpers = helpers }
    body |> ctx.Render
    sb.ToString()

let dust source data =
    dustExec (parse source) "" data null

let dustG source glob data =
    dustExec (parse source) "" data (json glob)

let dustReg name source data =
    let body = parse source
    cache.[name] <- (DateTime.Now, body)
    dustExec body name data null

let named name source =
    let body = parse source
    cache.[name] <- (DateTime.Now, body)

let shouldEqual (x: 'a) (y: 'a) = 
    Assert.AreEqual(x, y, sprintf "Expected: %A\nActual: %A" x y)

let shouldBeSome (x: string) (y: obj option) = 
    Assert.AreEqual(x, y.Value :?> string, sprintf "Expected: %A\nActual: %A" x y)

let shouldBeSomeS (x: string) (y: string option) = 
    Assert.AreEqual(x, y.Value, sprintf "Expected: %A\nActual: %A" x y)

let save (exp: string) (out: string)=
#if DEBUG
    File.WriteAllText(@"d:\out.txt", out)
    File.WriteAllText(@"d:\exp.txt", exp)
#endif
    ()

let expect a b =
    save a b
    shouldEqual a b