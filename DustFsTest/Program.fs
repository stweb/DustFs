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
    let ctx = { Context.defaults with _w = new StringWriter(sb); _templateDir = __SOURCE_DIRECTORY__ + """\null\""" ; data = data}
    body |> List.iter(fun p -> render ctx [] p)
    sb.ToString() 

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

module R01_DustFs =       

    [<Test>]
    let ``regex should parse tag into name,ctx,kv`` () =
        let name, ctx, kv = parseKeyValue """test type="primary" name="add_product" label="action.order.now" test2=hallo """
        name |> should equal "test"
        ctx  |> should equal None
        kv.TryFindProp "type"  |> shouldBeSome "primary"
        kv.TryFindProp "name"  |> shouldBeSome "add_product"
        kv.TryFindProp "label" |> shouldBeSome "action.order.now"
        kv.TryFindProp "test2" |> shouldBeSome "hallo"

module R02_CoreTests =

    // javascript-special characters in template names shouldn't break things
    [<Test>]
    let ``javascript-special characters in template names shouldn't break things`` () =
      empty
      |> dust  "confusing \" \n \' \u2028 \u2029 template name\\"
               "Hello World!"
      |> should equal "Hello World!"

    // === SUITE ===core tests
    // SKIPPED streaming render
    // should test basic text rendering
    [<Test>]
    let ``should test basic text rendering`` () =
      empty
      |> dust  "hello_world"
               "Hello World!"
      |> should equal "Hello World!"

    // should test a basic reference
    [<Test>]
    let ``should test a basic reference`` () =
      json "{\"one\":0}"
      |> dust  "reference"
               "{?one}{one}{/one}"
      |> should equal "0"

    // should test an implicit array
    [<Test>]
    let ``should test an implicit array`` () =
      json "{\"names\":[\"Moe\",\"Larry\",\"Curly\"]}"
      |> dust  "implicit array"
               "{#names}{.}{~n}{/names}"
      |> should equal "Moe\nLarry\nCurly\n"


    // should test base template
    [<Test>]
    let ``should test base and child template`` () =
      empty
      |> dustReg  "base_template"
               "Start{~n}{+title}Base Title{/title}{~n}{+main}Base Content{/main}{~n}End"
      |> should equal "Start\nBase Title\nBase Content\nEnd"


      json "{\"xhr\":false}"
      |> dust "child_template"
               "{^xhr}{>base_template/}{:else}{+main/}{/xhr}{<title}Child Title{/title}{<main}Child Content{/main}"
      |> should equal "Start\nChild Title\nChild Content\nEnd"

    // should test comments
    [<Test>]
    let ``should test comments`` () =
      empty
      |> dust  "comments"
               "{!\n  Multiline\n  {#foo}{bar}{/foo}\n!}\n{!before!}Hello{!after!}"
      |> should equal "Hello"

    // should test escaped characters
    [<Test>]
    let ``should test escaped characters`` () =
      json "{\"safe\":\"<script>alert(\'Hello!\')</script>\",\"unsafe\":\"<script>alert(\'Goodbye!\')</script>\"}"
      |> dust  "filter un-escape"
               "{safe|s}{~n}{unsafe}"
      |> should equal "<script>alert(\'Hello!\')</script>\n&lt;script&gt;alert(&#39;Goodbye!&#39;)&lt;/script&gt;"

    // should render the helper with missing global context
    [<Test>]
    let ``should render the helper with missing global context`` () =
      json "{}"
      |> dust  "makeBase_missing_global"
               "{#helper}{/helper}"
      |> should equal ""

module T02_CoreTests =

    // should render the template name
    [<Test>]
    [<Ignore "requires JS helper">]
    let ``should render the template name`` () =
      json "{}"
      |> dust  "global_template"
               "{#helper foo=\"bar\" boo=\"boo\"} {/helper}"
      |> should equal "global_template"

    // should render the template name with paths
    [<Test>]
    [<Ignore "requires JS helper">]   
    let ``should render the template name with paths`` () =
      json "{}"
      |> dust  "apps/test/foo.tl&v=0.1"
               "{#helper foo=\"bar\" boo=\"boo\" template=\"tl/apps/test\"} {/helper}"
      |> should equal "apps/test/foo.tl&v=0.1"

    // should test renaming a key
    [<Test>]
    [<Ignore "implement renaming">]
    let ``should test renaming a key`` () =
      json "{\"root\":\"Subject\",\"person\":{\"name\":\"Larry\",\"age\":45}}"
      |> dust  "inline param from outer scope"
               "{#person foo=root}{foo}: {name}, {age}{/person}"
      |> should equal "Subject: Larry, 45"

    // should test force a key
    [<Test>]
    let ``should test force a key`` () =
      json "{\"root\":\"Subject\",\"person\":{\"name\":\"Larry\",\"age\":45}}"
      |> dust  "force local"
               "{#person}{.root}: {name}, {age}{/person}"
      |> should equal ": Larry, 45"

    // should test escape_pragma
    [<Test>]
    [<Ignore "implement{%esc}">]
    let ``should test escape_pragma`` () =
      json "{\"unsafe\":\"<script>alert(\'Goodbye!\')</script>\"}"
      |> dust  "escape pragma"
               "{%esc:s}\n  {unsafe}{~n}\n  {%esc:h}\n    {unsafe}\n  {/esc}\n{/esc}"
      |> should equal "<script>alert(\'Goodbye!\')</script>\n&lt;script&gt;alert(&#39;Goodbye!&#39;)&lt;/script&gt;"

    // . creating a block
    [<Test>]
    [<Ignore("TODO implement ad-hoc blocks")>]
    let ``dot creating a block`` () =
      json "{\"name\":\"me\"}"
      |> dust  "use . for creating a block and set params"
               "{#. test=\"you\"}{name} {test}{/.}"
      |> should equal "me you"

    // should functions in context
    [<Test>]
    [<Ignore "requires JS in context">]
    let ``should functions in context`` () =
      json "{}"
      |> dust  "functions in context"
               "Hello {type} World!"
      |> should equal "Hello Sync World!"

    // should test functions in context
    [<Test>]
    [<Ignore "requires JS in context">]
    let ``should test functions in context`` () =
      json "{}"
      |> dust  "async functions in context"
               "Hello {type} World!"
      |> should equal "Hello Async World!"

    // should test sync chunk write
    [<Test>]
    [<Ignore "requires JS in context">]
    let ``should test sync chunk write`` () =
      json "{}"
      |> dust  "sync chunk write test"
               "Hello {type} World!"
      |> should equal "Hello Chunky World!"

    // should setup base template for next test. hi should not be part of base block name
    [<Test>]
    let ``should setup base template for next test; hi should not be part of base block name`` () =
      empty
      |> dustReg "issue322"
               "hi{+\"{name}\"/}"
      |> should equal "hi"

      empty
      |> dust  "issue322 use base template picks up prefix chunk data"
               "{>issue322 name=\"abc\"/}{<abc}ABC{/abc}"
      |> should equal "hiABC"

    // should test recursion
    [<Test>]
    //[<Ignore "TODO fix recursion">]
    let ``should test recursion`` () =
      json "{\"name\":\"1\",\"kids\":[{\"name\":\"1.1\",\"kids\":[{\"name\":\"1.1.1\"}]}]}"
      |> dustReg  "recursion" 
               "{name}{~n}{#kids}{>recursion:./}{/kids}"        
      |> should equal "1\n1.1\n1.1.1\n"

    // context.resolve() taps parameters from the context
    [<Test>]
    [<Ignore "Requires JS Context">]
    let ``context_resolve() taps parameters from the context`` () =
      json "{\"baz\":\"baz\",\"ref\":\"ref\"}"
      |> dust  "context.resolve"
               "{#foo bar=\"{baz} is baz \" literal=\"literal \" func=func chunkFunc=\"{chunkFunc}\" indirectChunkFunc=indirectChunkFunc ref=ref }Fail{/foo}"
      |> should equal "baz is baz literal func chunk indirect ref"

    // should test the context
    [<Test>]
    [<Ignore "Requires JS Context">]
    let ``should test the context`` () =
      json "{\"projects\":[{\"name\":\"Mayhem\"},{\"name\":\"Flash\"},{\"name\":\"Thunder\"}]}"
      |> dust  "context"
               "{#list:projects}{name}{:else}No Projects!{/list}"
      |> should equal "<ul>\n<li>Mayhem</li>\n<li>Flash</li>\n<li>Thunder</li>\n</ul>"

    // should allow pushing and popping a context    
    [<Test>]
    [<Ignore "Requires JS Context">]
    let ``should allow pushing and popping a context`` () =
      json "{}"
      |> dust  "context push / pop"
               "{#helper}{greeting} {firstName} {lastName}{.}{/helper}"
      |> should equal "Hello Dusty Dusterson!"

    // should allow cloning a context
    [<Test>]
    [<Ignore "Requires JS Context">]
    let ``should allow cloning a context`` () =
      json "{}"
      |> dust  "context clone"
               "{#helper}{greeting} {firstName} {lastName}{/helper}"
      |> should equal "Hello Dusty Dusterson"

module R03_TruthyFalsy =

    // === SUITE ===truth/falsy tests

    // should test for false in the context, evaluated and prints nothing
    [<Test>]
    let ``should test for false in the context, evaluated and prints nothing`` () =
      json "{\"false\":false}"
      |> dust  "false value in context is treated as empty, same as undefined"
               "{false}"
      |> should equal ""

    // should test for numeric zero in the context, prints the numeric zero
    [<Test>]
    let ``should test for numeric zero in the context, prints the numeric zero`` () =
      json "{\"zero\":0}"
      |> dust  "numeric 0 value in context is treated as non empty"
               "{zero}"
      |> should equal "0"

    // should test emptyString, prints nothing
    [<Test>]
    let ``should test emptyString, prints nothing`` () =
      json "{\"emptyString\":\"\"}"
      |> dust  "empty string context is treated as empty"
               "{emptyString}"
      |> should equal ""

    // should test emptyString single quoted, prints nothing
    [<Test>]
    let ``should test emptyString single quoted, prints nothing`` () =
      json "{\"emptyString\":\"\"}"
      |> dust  "empty string, single quoted in context is treated as empty"
               "{emptyString}"
      |> should equal ""

    // should test null in the context treated as empty
    [<Test>]
    let ``should test null in the context treated as empty`` () =
      json "{\"NULL\":null}"
      |> dust  "null in the context treated as empty"
               "{NULL}"
      |> should equal ""

    // should test undefined in the context treated as empty
    [<Test>]
    let ``should test undefined in the context treated as empty`` () =
      json "{}"
      |> dust  "undefined in the context treated as empty"
               "{UNDEFINED}"
      |> should equal ""

    // should test string undefined in the context as non empty
    [<Test>]
    let ``should test string undefined in the context as non empty`` () =
      json "{\"UNDEFINED\":\"undefined\"}"
      |> dust  "undefined string in the context treated as non empty"
               "{UNDEFINED}"
      |> should equal "undefined"

    // should test null as empty in exists section
    [<Test>]
    let ``should test null as empty in exists section`` () =
      json "{\"scalar\":null}"
      |> dust  "null is treated as empty in exists"
               "{?scalar}true{:else}false{/scalar}"
      |> should equal "false"

    // should test null treated as empty in exists
    [<Test>]
    let ``should test null treated as empty in exists`` () =
      json "{}"
      |> dust  "undefined is treated as empty in exists"
               "{?scalar}true{:else}false{/scalar}"
      |> should equal "false"

    // should test null as truthy in not exists
    [<Test>]
    let ``should test null as truthy in not exists`` () =
      json "{\"scalar\":null}"
      |> dust  "null is treated as truthy in not exists"
               "{^scalar}true{:else}false{/scalar}"
      |> should equal "true"

    // should test undefined as truthy in not exists
    [<Test>]
    let ``should test undefined as truthy in not exists`` () =
      json "{}"
      |> dust  "undefined is treated as truthy in not exists"
               "{^scalar}true{:else}false{/scalar}"
      |> should equal "true"

    // should test null treated as empty in exists
    [<Test>]
    let ``should test undefined treated as empty in exists`` () =
      json "{}"
      |> dust  "undefined is treated as empty in exists"
               "{?scalar}true{:else}false{/scalar}"
      |> should equal "false"

module R04_ScalarData =

    // === SUITE ===scalar data tests
    // should test for a scalar null in a # section
    [<Test>]
    let ``should test for a scalar null in a # section`` () =
      json "{\"scalar\":null}"
      |> dust  "scalar null in a # section"
               "{#scalar}true{:else}false{/scalar}"
      |> should equal "false"

    // should test for a scalar numeric 0 in a # section
    [<Test>]
    let ``should test for a scalar numeric 0 in a # section`` () =
      json "{\"scalar\":0}"
      |> dust  "scalar numeric 0 in a # section"
               "{#scalar}true{:else}false{/scalar}"
      |> should equal "true"

    // should test for a scalar numeric non-zero in a # section
    [<Test>]
    let ``should test for a scalar numeric non-zero in a # section`` () =
      json "{\"scalar\":42}"
      |> dust  "scalar numeric non-zero in a # section"
               "{#scalar}true{:else}false{/scalar}"
      |> should equal "true"

    // should test for a scalar string in a # section
    [<Test>]
    let ``should test for a scalar string in a # section`` () =
      json "{\"scalar\":\"abcde\"}"
      |> dust  "scalar non empty string in a # section"
               "{#scalar}true{:else}false{/scalar}"
      |> should equal "true"

    // should test for a scalar string in a # section
    [<Test>]
    let ``should test for a scalar string in a # section 2`` () =
      json "{\"scalar\":\"abcde\"}"
      |> dust  "scalar non empty string in a # section"
               "{#scalar}{.}{:else}false{/scalar}"
      |> should equal "abcde"

    // should test a missing/undefined scalar value
    [<Test>]
    let ``should test a missing/undefined scalar value`` () =
      json "{\"foo\":0}"
      |> dust  "missing scalar value"
               "{#scalar}true{:else}false{/scalar}"
      |> should equal "false"

    // shoud test for scalar true value in the # section
    [<Test>]
    let ``shoud test for scalar true value in the # section`` () =
      json "{\"scalar\":true}"
      |> dust  "scalar true value in the # section"
               "{#scalar}true{:else}false{/scalar}"
      |> should equal "true"

    // shoud test for scalar false value in the # section
    [<Test>]
    let ``shoud test for scalar false value in the # section`` () =
      json "{\"scalar\":false}"
      |> dust  "scalar false value in the # section"
               "{#scalar}true{:else}false{/scalar}"
      |> should equal "false"

    // should test scalar values true and false are supported in # nor else blocks
    [<Test>]
    let ``should test scalar values true and false are supported in # nor else blocks`` () =
      json "{\"foo\":true,\"bar\":false}"
      |> dust  "scalar values true and false are supported in # nor else blocks "
               "{#foo}foo,{~s}{:else}not foo,{~s}{/foo}{#bar}bar!{:else}not bar!{/bar}"
      |> should equal "foo, not bar!"

module R05_EmptyData =

    // === SUITE ===empty data tests
    // empty array is treated as empty in exists
    [<Test>]
    let ``empty array is treated as empty in exists`` () =
      json "{\"array\":[]}"
      |> dust  "empty array is treated as empty in exists"
               "{?array}true{:else}false{/array}"
      |> should equal "false"

    // empty {} is treated as non empty in exists
    [<Test>]
    let ``empty {} is treated as non empty in exists`` () =
      json "{\"object\":{}}"
      |> dust  "empty {} is treated as non empty in exists"
               "{?object}true{:else}false{/object}"
      |> should equal "true"

    // empty array is treated as empty in a section
    [<Test>]
    let ``empty array is treated as empty in a section`` () =
      json "{\"array\":[]}"
      |> dust  "empty array is treated as empty in a section"
               "{#array}true{:else}false{/array}"
      |> should equal "false"

    // empty {} is treated as non empty
    [<Test>]
    let ``empty {} is treated as non empty`` () =
      json "{\"object\":{}}"
      |> dust  "empty {} is treated as non empty in a section"
               "{#object}true{:else}false{/object}"
      |> should equal "true"

    // non-empty array in a reference
    [<Test>]
    let ``non-empty array in a reference`` () =
      json "{\"array\":[\"1\",\"2\"]}"
      |> dust  "non-empty array in a reference"
               "{array}"
      |> should equal "1,2"

    // should test null string in the context treated as non empty
    [<Test>]
    let ``should test null string in the context treated as non empty`` () =
      json "{\"NULL\":\"null\"}"
      |> dust  "null string in the context treated as non empty"
               "{NULL}"
      |> should equal "null"

    // should test for string zero in the context, prints zero
    [<Test>]
    let ``should test for string zero in the context, prints zero`` () =
      json "{\"zero\":\"0\"}"
      |> dust  "string 0 value in context is treated as non empty"
               "{zero}"
      |> should equal "0"

    // should test an empty array
    [<Test>]
    let ``should test an empty array`` () =
      json "{\"title\":\"Sir\",\"names\":[]}"
      |> dust  "empty array"
               "{#names}{title} {name}{~n}{/names}"
      |> should equal ""

    // should output nothing, but no error should fire
    [<Test>]
    let ``should output nothing, but no error should fire`` () =
      json "{}"
      |> dust  "empty params in helper"
               "{#emptyParamHelper}{/emptyParamHelper}"
      |> should equal ""

[<Ignore("TODO")>]
module T06_ArrayIndexAccess =

    // === SUITE ===array/index-access tests
    // should test an array
    [<Test>]
    let ``should test an array`` () =
      json "{\"title\":\"Sir\",\"names\":[{\"name\":\"Moe\"},{\"name\":\"Larry\"},{\"name\":\"Curly\"}]}"
      |> dust  "array"
               "{#names}{title} {name}{~n}{/names}"
      |> should equal "Sir Moe\nSir Larry\nSir Curly\n"

    // should return a specific array element by index when element value is a primitive
    [<Test>]
    let ``should return a specific array element by index when element value is a primitive`` () =
      json "{\"do\":{\"re\":[\"hello!\",\"bye!\"]}}"
      |> dust  "accessing array element by index when element value is a primitive"
               "{do.re[0]}"
      |> should equal "hello!"

    // should return a specific array element by index when element value is a object
    [<Test>]
    let ``should return a specific array element by index when element value is a object`` () =
      json "{\"do\":{\"re\":[{\"mi\":\"hello!\"},\"bye!\"]}}"
      |> dust  "accessing array by index when element value is a object"
               "{do.re[0].mi}"
      |> should equal "hello!"

    // should return a specific array element by index when element is a nested object
    [<Test>]
    let ``should return a specific array element by index when element is a nested object`` () =
      json "{\"do\":{\"re\":[{\"mi\":[\"one\",{\"fa\":\"hello!\"}]},\"bye!\"]}}"
      |> dust  "accessing array by index when element is a nested object"
               "{do.re[0].mi[1].fa}"
      |> should equal "hello!"

    // should return a specific array element by index when element is list of primitives
    [<Test>]
    let ``should return a specific array element by index when element is list of primitives`` () =
      json "{\"do\":[\"lala\",\"lele\"]}"
      |> dust  "accessing array by index when element is list of primitives"
               "{do[0]}"
      |> should equal "lala"

    // should return a specific array element using the current context
    [<Test>]
    let ``should return a specific array element using the current context`` () =
      json "{\"list3\":[[{\"biz\":\"123\"}],[{\"biz\":\"345\"}]]}"
      |> dust  "accessing array inside a loop using the current context"
               "{#list3}{.[0].biz}{/list3}"
      |> should equal "123345"

    // array: reference $idx in iteration on objects
    [<Test>]
    let ``array: reference $idx in iteration on objects`` () =
      json "{\"title\":\"Sir\",\"names\":[{\"name\":\"Moe\"},{\"name\":\"Larry\"},{\"name\":\"Curly\"}]}"
      |> dust  "array: reference $idx in iteration on objects"
               "{#names}({$idx}).{title} {name}{~n}{/names}"
      |> should equal "(0).Sir Moe\n(1).Sir Larry\n(2).Sir Curly\n"

    // test array: reference $len in iteration on objects
    [<Test>]
    let ``test array: reference $len in iteration on objects`` () =
      json "{\"title\":\"Sir\",\"names\":[{\"name\":\"Moe\"},{\"name\":\"Larry\"},{\"name\":\"Curly\"}]}"
      |> dust  "array: reference $len in iteration on objects"
               "{#names}Size=({$len}).{title} {name}{~n}{/names}"
      |> should equal "Size=(3).Sir Moe\nSize=(3).Sir Larry\nSize=(3).Sir Curly\n"

    // test array reference $idx in iteration on simple types
    [<Test>]
    let ``test array reference $idx in iteration on simple types`` () =
      json "{\"title\":\"Sir\",\"names\":[\"Moe\",\"Larry\",\"Curly\"]}"
      |> dust  "array reference $idx in iteration on simple type"
               "{#names}({$idx}).{title} {.}{~n}{/names}"
      |> should equal "(0).Sir Moe\n(1).Sir Larry\n(2).Sir Curly\n"

    // test array reference $len in iteration on simple types
    [<Test>]
    let ``test array reference $len in iteration on simple types`` () =
      json "{\"title\":\"Sir\",\"names\":[\"Moe\",\"Larry\",\"Curly\"]}"
      |> dust  "array reference $len in iteration on simple type"
               "{#names}Size=({$len}).{title} {.}{~n}{/names}"
      |> should equal "Size=(3).Sir Moe\nSize=(3).Sir Larry\nSize=(3).Sir Curly\n"

    // test array reference $idx/$len on empty array case
    [<Test>]
    let ``test array reference $idx $len on empty array case`` () =
      json "{\"title\":\"Sir\",\"names\":[]}"
      |> dust  "array reference $idx/$len on empty array case"
               "{#names}Idx={$idx} Size=({$len}).{title} {.}{~n}{/names}"
      |> should equal ""

    // test array reference $idx/$len on single element case
    [<Test>]
    let ``test array reference $idx $len on single element case`` () =
      json "{\"name\":\"Just one name\"}"
      |> dust  "array reference $idx/$len on single element case (scalar case)"
               "{#name}Idx={$idx} Size={$len} {.}{/name}"
      |> should equal "Idx= Size= Just one name"

    // test array reference $idx/$len {#.} section case
    [<Test>]
    let ``test array reference $idx $len section case`` () =
      json "{\"names\":[\"Moe\",\"Larry\",\"Curly\"]}"
      |> dust  "array reference $idx/$len {#.} section case"
               "{#names}{#.}{$idx}{.} {/.}{/names}"
      |> should equal "0Moe 1Larry 2Curly "

    // test array reference $idx/$len not changed in nested object
    [<Test>]
    let ``test array reference $idx $len not changed in nested object`` () =
      json "{\"results\":[{\"info\":{\"name\":\"Steven\"}},{\"info\":{\"name\":\"Richard\"}}]}"
      |> dust  "array reference $idx/$len not changed in nested object"
               "{#results}{#info}{$idx}{name}-{$len} {/info}{/results}"
      |> should equal "0Steven-2 1Richard-2 "

    // test array reference $idx/$len nested loops
    [<Test>]
    let ``test array reference $idx $len nested loops`` () =
      json "{\"A\":[{\"B\":[{\"C\":[\"Ca1\",\"C2\"]},{\"C\":[\"Ca2\",\"Ca22\"]}]},{\"B\":[{\"C\":[\"Cb1\",\"C2\"]},{\"C\":[\"Cb2\",\"Ca2\"]}]}]}"
      |> dust  "array reference $idx/$len nested loops"
               "{#A}A loop:{$idx}-{$len},{#B}B loop:{$idx}-{$len}C[0]={.C[0]} {/B}A loop trailing: {$idx}-{$len}{/A}"
      |> should equal "A loop:0-2,B loop:0-2C[0]=Ca1 B loop:1-2C[0]=Ca2 A loop trailing: 0-2A loop:1-2,B loop:0-2C[0]=Cb1 B loop:1-2C[0]=Cb2 A loop trailing: 1-2"

    // should test the array reference access with idx
    [<Test>]
    let ``should test the array reference access with idx`` () =
      json "{\"list4\":[{\"name\":\"Dog\",\"number\":[1,2,3]},{\"name\":\"Cat\",\"number\":[4,5,6]}]}"
      |> dust  "using idx in array reference Accessing"
               "{#list4} {name} {number[$idx]} {$idx}{/list4}"
      |> should equal " Dog 1 0 Cat 5 1"

    // should test the array reference access with len
    [<Test>]
    let ``should test the array reference access with len`` () =
      json "{\"list4\":[{\"name\":\"Dog\",\"number\":[1,2,3]},{\"name\":\"Cat\",\"number\":[4,5,6]}]}"
      |> dust  "using len in array reference Accessing"
               "{#list4} {name} {number[$len]}{/list4}"
      |> should equal " Dog 3 Cat 6"

    // should test the array reference access with idx and current context
    [<Test>]
    let ``should test the array reference access with idx and current context`` () =
      json "{\"list3\":[[{\"biz\":\"123\"}],[{\"biz\":\"345\"},{\"biz\":\"456\"}]]}"
      |> dust  "using idx in array reference Accessing"
               "{#list3}{.[$idx].biz}{/list3}"
      |> should equal "123456"

    // should test the array reference access with len and current context
    [<Test>]
    let ``should test the array reference access with len and current context`` () =
      json "{\"list3\":[[{\"idx\":\"0\"},{\"idx\":\"1\"},{\"idx\":\"2\"}],[{\"idx\":\"0\"},{\"idx\":\"1\"},{\"idx\":\"2\"}]]}"
      |> dust  "using len in array reference Accessing"
               "{#list3}{.[$len].idx}{/list3}"
      |> should equal "22"

    // should test double nested array and . reference: issue #340
    [<Test>]
    let ``should test double nested array and dot reference: issue #340`` () =
      json "{\"test\":[[1,2,3]]}"
      |> dust  "using idx in double nested array"
               "{#test}{#.}{.}i:{$idx}l:{$len},{/.}{/test}"
      |> should equal "1i:0l:3,2i:1l:3,3i:2l:3,"

    // should test using a multilevel reference as a key in array access
    [<Test>]
    let ``should test using a multilevel reference as a key in array access`` () =
      json "{\"loop\":{\"array\":{\"thing\":{\"sub\":1,\"sap\":2},\"thing2\":\"bar\"}},\"key\":{\"foo\":\"thing\"}}"
      |> dust  "using a nested key as a reference for array index access"
               "{#loop.array[key.foo].sub}{.}{/loop.array[key.foo].sub}"
      |> should equal "1"

    // should HTML-encode stringified arrays referenced directly
    [<Test>]
    let ``should HTML-encode stringified arrays referenced directly`` () =
      json "{\"array\":[\"You & I\",\" & Moe\"]}"
      |> dust  "Outputting an array calls toString and HTML-encodes"
               "{array}"
      |> should equal "You &amp; I, &amp; Moe"

module R07_ObjectTests =

    // === SUITE ===object tests
    // should test an object
    [<Test>]
    let ``should test an object`` () =
      json "{\"root\":\"Subject\",\"person\":{\"name\":\"Larry\",\"age\":45}}"
      |> dust  "object"
               "{#person}{root}: {name}, {age}{/person}"
      |> should equal "Subject: Larry, 45"

    // should test an object path
    [<Test>]
    let ``should test an object path`` () =
      json "{\"foo\":{\"bar\":\"Hello!\"}}"
      |> dust  "path"
               "{foo.bar}"
      |> should equal "Hello!"

[<Ignore "Implement thenable/promises">]
module T07_ObjectTestsWithThenable =

    // should reserve an async chunk for a thenable reference
    [<Test>]
    let ``should reserve an async chunk for a thenable reference`` () =
      json "{\"magic\":{}}"
      |> dust  "thenable reference"
               "Eventually {magic}!"
      |> should equal "Eventually magic!"

    // undefined
    [<Test>]
    let ``undefined`` () =
      json "{\"rice-krispies\":{}}"
      |> dust  "thenable escape reference"
               "{rice-krispies} {rice-krispies|s}"
      |> should equal "Snap, Crackle &amp; Pop Snap, Crackle & Pop"

    // should deep-inspect a thenable reference
    [<Test>]
    let ``should deep-inspect a thenable reference`` () =
      json "{\"magic\":{}}"
      |> dust  "thenable deep reference"
               "Eventually {magic.ally.delicious}!"
      |> should equal "Eventually Lucky Charms!"

    // should deep-inspect a thenable reference but move on if it isn't there
    [<Test>]
    let ``should deep-inspect a thenable reference but move on if it isn't there`` () =
      json "{\"magic\":{}}"
      |> dust  "thenable deep reference that doesn\'t exist"
               "Eventually {magic.ally.disappeared}!"
      |> should equal "Eventually !"

    // should deep-inspect a thenable reference recursively
    [<Test>]
    let ``should deep-inspect a thenable reference recursively`` () =
      json "{\"magic\":{}}"
      |> dust  "thenable deep reference... this is just getting silly"
               "Eventually {magic.ally.delicious}!"
      |> should equal "Eventually Lucky Charms!"

    // should inspect a thenable reference but move on if it fails
    [<Test>]
    let ``should inspect a thenable reference but move on if it fails`` () =
      json "{\"magic\":{}}"
      |> dust  "thenable reference that fails"
               "Eventually {magic.ally.delicious}!"
      |> should equal "Eventually !"

    // should deep-inspect a thenable reference but move on if it fails
    [<Test>]
    let ``should deep-inspect a thenable reference but move on if it fails`` () =
      json "{\"magic\":{}}"
      |> dust  "thenable deep reference that fails"
               "Eventually {magic.ally.delicious}!"
      |> should equal "Eventually !"

    // should reserve an async section for a thenable
    [<Test>]
    let ``should reserve an async section for a thenable`` () =
      json "{\"promise\":{}}"
      |> dust  "thenable section"
               "{#promise}Eventually {magic}!{/promise}"
      |> should equal "Eventually magic!"

    // should iterate over an array resolved from a thenable
    [<Test>]
    let ``should iterate over an array resolved from a thenable`` () =
      json "{\"promise\":{}}"
      |> dust  "thenable resolves with array into reference"
               "{promise}"
      |> should equal "foo,bar,baz"

    // should iterate over an array resolved from a thenable
    [<Test>]
    let ``should iterate over an array resolved from a thenable 2`` () =
      json "{\"promise\":{}}"
      |> dust  "thenable resolves with array into section"
               "{#promise}{name}{/promise}"
      |> should equal "foobarbaz"

    // Should not render a thenable section with no body
    [<Test>]
    let ``Should not render a thenable section with no body`` () =
      json "{\"promise\":{}}"
      |> dust  "thenable empty section"
               "{#promise/}"
      |> should equal ""

    // should reserve an async section for a thenable returned from a function
    [<Test>]
    let ``should reserve an async section for a thenable returned from a function`` () =
      json "{}"
      |> dust  "thenable section from function"
               "{#promise}Eventually {magic}!{/promise}"
      |> should equal "Eventually magic!"

    // should reserve an async section for a deep-reference thenable
    [<Test>]
    let ``should reserve an async section for a deep-reference thenable`` () =
      json "{\"magic\":{}}"
      |> dust  "thenable deep section"
               "Eventually my {#magic.ally}{delicious}{/magic.ally} will come"
      |> should equal "Eventually my Lucky Charms will come"

    // should reserve an async section for a deep-reference thenable and not blow the stack
    [<Test>]
    let ``should reserve an async section for a deep-reference thenable and not blow the stack`` () =
      json "{\"prince\":\"Prince\",\"magic\":{}}"
      |> dust  "thenable deep section, traverse outside"
               "Eventually my {#magic.ally}{prince} {delicious} {charms}{/magic.ally} will come"
      |> should equal "Eventually my Prince Lucky Charms will come"

    // Dust helpers that return thenables are resolved in context
    [<Test>]
    let ``Dust helpers that return thenables are resolved in context`` () =
      empty
      |> dust  "thenable resolved by global helper"
               "{@promise resolve=\"helper\"}I am a big {.}!{/promise}"
      |> should equal "I am a big helper!"

    // Dust helpers that return thenables are rejected in context
    [<Test>]
    let ``Dust helpers that return thenables are rejected in context`` () =
      empty
      |> dust  "thenable rejected by global helper"
               "{@promise reject=\"error\"}I am a big helper!{:error}I am a big {.}!{/promise}"
      |> should equal "I am a big error!"

    // rejected thenable reference logs
    [<Test>]
    let ``rejected thenable reference logs`` () =
      json "{\"promise\":{}}"
      |> dust  "thenable error"
               "{promise}"
      |> should equal "undefined"

    // rejected thenable renders error block
    [<Test>]
    let ``rejected thenable renders error block`` () =
      json "{\"promise\":{}}"
      |> dust  "thenable error with error block"
               "{#promise}No magic{:error}{message}{/promise}"
      |> should equal "promise error"

[<Ignore "Implement streams">]
module T07_ObjectTestsWithStreams =

    // should reserve an async chunk for a stream reference
    [<Test>]
    let ``should reserve an async chunk for a stream reference`` () =
      json "{}"
      |> dust  "stream"
               "Stream of {stream}..."
      |> should equal "Stream of consciousness..."

    // should respect filters set on stream references
    [<Test>]
    let ``should respect filters set on stream references`` () =
      json "{}"
      |> dust  "stream escaping"
               "{polluted|s} {polluted}"
      |> should equal "<&> &lt;&amp;&gt;"

    // should abort the stream if it raises an error
    [<Test>]
    let ``should abort the stream if it raises an error`` () =
      json "{}"
      |> dust  "stream error"
               "{stream}..."
      |> should equal "Everything is..."

    // should reserve an async section for a stream
    [<Test>]
    let ``should reserve an async section for a stream`` () =
      json "{}"
      |> dust  "stream section"
               "Pour {#molecule}{atom}{num}{/molecule} in the glass"
      |> should equal "Pour H2O in the glass"

    // should reserve an async chunk for a stream reference and abort if the stream errors
    [<Test>]
    let ``should reserve an async chunk for a stream reference and abort if the stream errors`` () =
      json "{}"
      |> dust  "stream section error"
               "Pour {#molecule}{atom}{num}{:error}{message}{/molecule} in the glass"
      |> should equal "Pour H2O... no! in the glass"

    // should render streams found while iterating over an array
    [<Test>]
    let ``should render streams found while iterating over an array`` () =
      json "{\"streams\":[null,null,null]}"
      |> dust  "array of streams"
               "{#streams}{.} {/streams}"
      |> should equal "Danube Rhine Seine "

    // should seamlessly mix asynchronous data sources
    [<Test>]
    let ``should seamlessly mix asynchronous data sources`` () =
      json "{\"water\":{}}"
      |> dust  "promise a stream and stream a promise"
               "Little Bobby drank and drank, and then he drank some more. But what he thought was {water} was {sulfuric_acid}!"
      |> should equal "Little Bobby drank and drank, and then he drank some more. But what he thought was H2O was H2SO4!"

    // should not treat MongoDB documents as streams
    [<Test>]
    let ``should not treat MongoDB documents as streams`` () =
      json "{\"mongo\":{\"name\":\"Mongo\"}}"
      |> dust  "MongoDB-like Document is not a stream"
               "{#mongo}{name}{/mongo}"
      |> should equal "Mongo"

    // stream section with no body should not render
    [<Test>]
    let ``stream section with no body should not render`` () =
      json "{}"
      |> dust  "Stream section with no body should not render"
               "{#stream/}"
      |> should equal ""

module R06_Conditional =

    // === SUITE ===conditional tests
    // should test conditional tags
    [<Test>]
    let ``should test conditional tags`` () =
      json "{\"tags\":[],\"likes\":[\"moe\",\"larry\",\"curly\",\"shemp\"]}"
      |> dust  "conditional"
               "{?tags}<ul>{~n}{#tags}{~s} <li>{.}</li>{~n}{/tags}</ul>{:else}No Tags!{/tags}{~n}{^likes}No Likes!{:else}<ul>{~n}{#likes}{~s} <li>{.}</li>{~n}{/likes}</ul>{/likes}"
      |> should equal "No Tags!\n<ul>\n  <li>moe</li>\n  <li>larry</li>\n  <li>curly</li>\n  <li>shemp</li>\n</ul>"

    // should test else block when array empty
    [<Test>]
    let ``should test else block when array empty`` () =
      json "{\"foo\":[]}"
      |> dust  "empty else block"
               "{#foo}full foo{:else}empty foo{/foo}"
      |> should equal "empty foo"

module R07_NestedPaths =
    // === SUITE ===nested path tests
    // should test the leading dot behavior in local mode
    [<Test>]
    [<Ignore "fix . reference section parsing">]
    let ``should test the leading dot behavior in local mode`` () =
      json "{\"name\":\"List of people\",\"age\":\"8 hours\",\"people\":[{\"name\":\"Alice\"},{\"name\":\"Bob\",\"age\":42}]}"
      |> dust  "Verify local mode leading dot path in local mode"
               "{#people}{.name} is {?.age}{.age} years old.{:else}not telling us their age.{/age}{/people}"
      |> should equal "Alice is not telling us their age.Bob is 42 years old."

    // should work when value at end of path is falsey
    [<Test>]
    let ``should work when value at end of path is falsey`` () =
      json "{\"foo\":{\"bar\":0}}"
      |> dust  "Standard dotted path with falsey value. Issue 317"
               "{foo.bar}"
      |> should equal "0"

    // should test usage of dotted path resolution up context
    [<Test>]
    let ``should test usage of dotted path resolution up context`` () =
      json "{\"data\":{\"A\":{\"name\":\"Al\",\"list\":[{\"name\":\"Joe\"},{\"name\":\"Mary\"}],\"B\":{\"name\":\"Bob\",\"Blist\":[\"BB1\",\"BB2\"]}}}}"
      |> dust  "dotted path resolution up context"
               "{#data.A.list}Aname{data.A.name}{/data.A.list}"
      |> should equal "AnameAlAnameAl"

    // should test usage of dotted path resolution up context
    [<Test>]
    let ``should test usage of dotted path resolution up context 2`` () =
      json "{\"data\":{\"A\":{\"name\":\"Al\",\"list\":[{\"name\":\"Joe\"},{\"name\":\"Mary\"}],\"B\":{\"name\":\"Bob\",\"Blist\":[\"BB1\",\"BB2\"]}}}}"
      |> dust  "dotted path resolution up context 2"
               "{#data.A.B.Blist}Aname{data.A.name}{/data.A.B.Blist}"
      |> should equal "AnameAlAnameAl"

    // should test usage of dotted path resolution up context
    [<Test>]
    let ``should test usage of dotted path resolution up context 3`` () =
      json "{\"data\":{\"A\":{\"name\":\"Al\",\"list\":[{\"name\":\"Joe\"},{\"name\":\"Mary\"}],\"B\":{\"name\":\"Bob\",\"Blist\":[\"BB1\",\"BB2\"]}},\"C\":{\"name\":\"cname\"}}}"
      |> dust  "dotted path resolution without explicit context"
               "{#data.A}Aname{name}{data.C.name}{/data.A}"
      |> should equal "AnameAlcname"

    // should test explicit context blocks looking further up stack
    [<Test>]
    [<Ignore "TODO Implement explicit context">]
    let ``should test explicit context blocks looking further up stack`` () =
      json "{\"data\":{\"A\":{\"name\":\"Al\",\"list\":[{\"name\":\"Joe\"},{\"name\":\"Mary\"}],\"B\":{\"name\":\"Bob\",\"Blist\":[\"BB1\",\"BB2\"]}},\"C\":{\"name\":\"cname\"}}}"
      |> dust  "same as previous test but with explicit context"
               "{#data.A:B}Aname{name}{data.C.name}{/data.A}"
      |> should equal "AnameAl"

    // should test access global despite explicit context
    [<Test>]
    [<Ignore "TODO Implement global">]
    let ``should test access global despite explicit context`` () =
      json "{\"data\":{\"A\":{\"name\":\"Al\",\"list\":[{\"name\":\"Joe\"},{\"name\":\"Mary\"}],\"B\":{\"name\":\"Bob\",\"Blist\":[\"BB1\",\"BB2\"]}},\"C\":{\"name\":\"cname\"}}}"
      |> dust  "explicit context but gets value from global"
               "{#data.A:B}Aname{name}{glob.globChild}{/data.A}"
      |> should equal "AnameAltestGlobal"

    // should test nested usage of dotted path resolution
    [<Test>]
    let ``should test nested usage of dotted path resolution`` () =
      json "{\"data\":{\"A\":{\"name\":\"Al\",\"list\":[{\"name\":\"Joe\"},{\"name\":\"Mary\"}],\"B\":{\"name\":\"Bob\",\"Blist\":[\"BB1\"]}}}}"
      |> dust  "nested dotted path resolution"
               "{#data.A.list}{#data.A.B.Blist}{.}Aname{data.A.name}{/data.A.B.Blist}{/data.A.list}"
      |> should equal "BB1AnameAlBB1AnameAl"

    // Should find glob.globChild which is in context.global
    [<Test>]
    [<Ignore "TODO Implement global">]
    let ``Should find glob_globChild which is in context_global`` () =
      empty
      // base: { glob: { globChild: "testGlobal"} },        
      |> dust  "check nested ref in global works in global mode"
               "{glob.globChild}"
      |> should equal "testGlobal"

    // should test usage of dotted path resolution up context
    [<Test>]
    let ``should test usage of dotted path resolution up context 4`` () =
      json "{\"data\":{\"A\":{\"name\":\"Al\",\"B\":\"Ben\",\"C\":{\"namex\":\"Charlie\"}},\"C\":{\"name\":\"Charlie Sr.\"}}}"
      |> dust  "dotted path resolution up context with partial match in current context"
               "{#data}{#A}{C.name}{/A}{/data}"
      |> should equal ""

    // Should find glob.globChild which is in context.global
    [<Test>]
    [<Ignore "TODO Implement global">]
    let ``Should find glob_globChild which is in context_global 2`` () =
      json "{\"data\":{\"A\":{\"name\":\"Al\",\"B\":\"Ben\",\"C\":{\"namex\":\"Charlie\"}},\"C\":{\"namey\":\"Charlie Sr.\"}}}"
      |> dust  "check nested ref not found in global if partial match"
               "{#data}{#A}{C.name}{/A}{/data}"
      |> should equal ""

    // should test resolve correct 'this' 
    [<Test>]
    let ``should test resolve correct 'this'`` () =
      json "{\"person\":{\"firstName\":\"Peter\",\"lastName\":\"Jones\",\"fullName\":\"Peter Jones\"}}"
      |> dust  "method invocation"
               "Hello {person.fullName}"
      |> should equal "Hello Peter Jones"

    // should test resolve correct 'this' when invoking method
    [<Ignore "Requires JavaScript">]
    [<Test>]
    let ``should test resolve correct 'this' when invoking method`` () =
      json "{\"person\":{\"firstName\":\"Peter\",\"lastName\":\"Jones\", \"fullName\": function() {
                return this.firstName + ' ' + this.lastName;
            }}}"
      |> dust  "method invocation"
               "Hello {person.fullName}"
      |> should equal "Hello Peter Jones"

    // Should resolve path correctly
    [<Test>]
    [<Ignore "implement array index references">]
    let ``Should resolve path correctly`` () =
      json "{\"nulls\":[1,null,null,2],\"names\":[{\"name\":\"Moe\"},{\"name\":\"Curly\"}]}"
      |> dust  "check null values in section iteration don\'t break path resolution"
               "{#nulls}{names[0].name}{/nulls}"
      |> should equal "MoeMoeMoeMoe"

    // Should resolve path correctly
    [<Test>]
    let ``Should resolve path correctly 2`` () =
      json "{\"list\":[\"\",2,\"\"],\"a\":{\"b\":\"B\"}}"
      |> dust  "check falsey value in section iteration don\'t break path resolution"
               "{#list}{a.b}{/list}"
      |> should equal "BBB"

    // Should resolve path correctly
    [<Test>]
    let ``Should resolve path correctly 3`` () =
      json "{\"list\":[true,2,true],\"a\":{\"b\":\"B\"}}"
      |> dust  "check true value in section iteration are also OK"
               "{#list}{a.b}{/list}"
      |> should equal "BBB"

[<Ignore "TODO implement j filters">]
module T09_Filter =

    // === SUITE ===filter tests
    // should test the filter tag
    [<Test>]
    [<Ignore "requires JavaScript">]
    let ``should test the filter tag`` () =
      json "{\"bar\":\"bar\"}"
//              context:  {
//                    filter: function(chunk, context, bodies) {
//                      return chunk.tap(function(data) {
//                        return data.toUpperCase();
//                      }).render(bodies.block, context).untap();
//                    },
//
//                    bar: "bar"
//                  },
      |> dust  "filter"
               "{#filter}foo {bar}{/filter}"
      |> should equal "FOO BAR"

    // should escapeJs a string when using the j filter
    [<Test>]
    let ``should escapeJs a string when using the j filter`` () =
      json "{\"obj\":\"<script>\\\\testBS\\\\ \\rtestCR\\r \u2028testLS\u2028 \u2029testPS\u2029 \\ntestNL\\n \\ftestLF\\f \'testSQ\' \\ttestTB\\t /testFS/</script>\"}"
      |> dust  "escapeJs filter without DQ"
               "{obj|j|s}"
      |> should equal "<script>\\\\testBS\\\\ \\rtestCR\\r \\u2028testLS\\u2028 \\u2029testPS\\u2029 \\ntestNL\\n \\ftestLF\\f \\\'testSQ\\\' \\ttestTB\\t \\/testFS\\/<\\/script>"

    // should escapeJs a string with double quotes when using the j filter
    [<Test>]
    let ``should escapeJs a string with double quotes when using the j filter`` () =
      json "{\"obj\":\"\\\"testDQ\\\"\"}"
      |> dust  "escapeJs filter with only DQ"
               "{obj|j|s}"
      |> should equal "\\\"testDQ\\\""

    // should stringify a JSON literal when using the js filter
    [<Test>]
    let ``should stringify a JSON literal when using the js filter`` () =
      json "{\"obj\":{\"id\":1,\"name\":\"bob\",\"occupation\":\"construction\"}}"
      |> dust  "escapeJSON filter"
               "{obj|js|s}"
      |> should equal "{\"id\":1,\"name\":\"bob\",\"occupation\":\"construction\"}"

    // should escape bad characters when using the js filter
    [<Test>]
    let ``should escape bad characters when using the js filter`` () =
      json "{\"obj\":{\"name\":\"<<\u2028testLS \u2029testPS\"}}"
      |> dust  "escapeJSON filter with bad characters"
               "{obj|js|s}"
      |> should equal "{\"name\":\"\\u003c\\u003c\\u2028testLS \\u2029testPS\"}"

    // should objectify a JSON string when using the jp filter
    [<Test>]
    let ``should objectify a JSON string when using the jp filter`` () =
      json "{\"obj\":\"{\\\"id\\\":1,\\\"name\\\":\\\"bob\\\",\\\"occupation\\\":\\\"construction\\\"}\"}"
      |> dust  "JSON.parse filter"
               "{obj|jp}"
      |> should equal "[object Object]"

    // filters are passed the current context
    [<Test>]
    let ``filters are passed the current context`` () =
      json "{\"woo\":0,\"name\":\"Boo\",\"dust\":{\"woo\":5,\"name\":\"Dust\"}}"
      |> dust  "filter receives context"
               "{#dust}{name|woo}{/dust}"
      |> should equal "DUST!!!!!"

module R10_Partial =

    // === SUITE ===partial definitions
    // should test a basic replace in a template
    [<Test>]
    let ``should test a basic replace in a template`` () =
      json "{\"name\":\"Mick\",\"count\":30}"
      |> dust  "partial"
               "Hello {name}! You have {count} new messages."
      |> should equal "Hello Mick! You have 30 new messages."

    // should test a block with defaults
    [<Test>]
    let ``should test a block with defaults`` () =
      json "{\"name\":\"Mick\",\"count\":30}"
      |> dust  "partial_with_blocks"
               "{+header}default header {/header}Hello {name}! You have {count} new messages."
      |> should equal "default header Hello Mick! You have 30 new messages."

    // should test a blocks with no defaults
    [<Test>]
    let ``should test a blocks with no defaults`` () =
      json "{\"name\":\"Mick\",\"count\":30}"
      |> dust  "partial_with_blocks_and_no_defaults"
               "{+header/}Hello {name}! You have {count} new messages."
      |> should equal "Hello Mick! You have 30 new messages."

    // should test a template with missing helper
    [<Test>]
    let ``should test a template with missing helper`` () =
      empty
      |> dust  "partial_print_name"
               "{#helper}{/helper}"
      |> should equal ""

    // should test partial
    [<Test>]
    let ``should test partial`` () =
      empty
      |> dust  "nested_partial_print_name"
               "{>partial_print_name/}"
      |> should equal ""

    // should test nested partial
    [<Test>]
    let ``should test nested partial`` () =
      empty
      |> dust  "nested_nested_partial_print_name"
               "{>nested_partial_print_name/}"
      |> should equal ""

[<Ignore("TODO implement")>]
module T11_PartialParams =

    // === SUITE ===partial/params tests
    // should test partials
    [<Test>]
    let ``should test partials`` () =
      json "{\"name\":\"Jim\",\"count\":42,\"ref\":\"hello_world\"}"
      |> dust  "partials"
               "{>partial foo=0 /} {>\"hello_world\" foo=1 /} {>\"{ref}\" foo=2 /}"
      |> should equal "Hello Jim! You have 42 new messages. Hello World! Hello World!"

    // should test partial with an asynchronously-resolved template name
    [<Test>]
    let ``should test partial with an asynchronously-resolved template name`` () =
      json "{}"
      |> dust  "partial with async ref as name"
               "{>\"{ref}\" /}"
      |> should equal "Hello World!"

    // should test partial with context
    [<Test>]
    let ``should test partial with context`` () =
      json "{\"profile\":{\"name\":\"Mick\",\"count\":30}}"
      |> dust  "partial with context"
               "{>partial:.profile/}"
      |> should equal "Hello Mick! You have 30 new messages."

    // partial with blocks, with no default values for blocks
    [<Test>]
    let ``partial with blocks, with no default values for blocks`` () =
      json "{\"name\":\"Mick\",\"count\":30}"
      |> dust  "partial with blocks, with no default values for blocks"
               "{>partial_with_blocks_and_no_defaults/}"
      |> should equal "Hello Mick! You have 30 new messages."

    // partial with blocks, with no default values for blocks, but override default values with inline partials
    [<Test>]
    let ``partial with blocks, with no default values for blocks, but override default values with inline partials`` () =
      json "{\"name\":\"Mick\",\"count\":30}"
      |> dust  "partial with blocks, with no default values for blocks, but override default values with inline partials"
               "{>partial_with_blocks_and_no_defaults/}{<header}override header {/header}"
      |> should equal "override header Hello Mick! You have 30 new messages."

    // partial with blocks, override default values with inline partials
    [<Test>]
    let ``partial with blocks, override default values with inline partials`` () =
      json "{\"name\":\"Mick\",\"count\":30}"
      |> dust  "partial with blocks, override default values with inline partials"
               "{>partial_with_blocks/}{<header}my header {/header}"
      |> should equal "my header Hello Mick! You have 30 new messages."

    // should test partial with inline params
    [<Test>]
    let ``should test partial with inline params`` () =
      json "{\"n\":\"Mick\",\"c\":30}"
      |> dust  "partial with inline params"
               "{>partial name=n count=\"{c}\"/}"
      |> should equal "Hello Mick! You have 30 new messages."

    // should test partial with inline params tree walk up
    [<Test>]
    let ``should test partial with inline params tree walk up`` () =
      json "{\"n\":\"Mick\",\"x\":30,\"a\":{\"b\":{\"c\":{\"d\":{\"e\":\"1\"}}}}}"
      |> dust  "partial with inline params tree walk up"
               "{#a}{#b}{#c}{#d}{>partial name=n count=\"{x}\"/}{/d}{/c}{/b}{/a}"
      |> should equal "Hello Mick! You have 30 new messages."

    // should test partial with inline params and context
    [<Test>]
    let ``should test partial with inline params and context`` () =
      json "{\"profile\":{\"n\":\"Mick\",\"c\":30}}"
      |> dust  "partial with inline params and context"
               "{>partial:profile name=\"{n}\" count=\"{c}\"/}"
      |> should equal "Hello Mick! You have 30 new messages."

    // should test partial with inline params and context tree walk up
    [<Test>]
    let ``should test partial with inline params and context tree walk up`` () =
      json "{\"profile\":{\"n\":\"Mick\",\"x\":30,\"a\":{\"b\":{\"c\":{\"d\":{\"e\":\"1\"}}}}}}"
      |> dust  "partial with inline params and context tree walk up"
               "{#profile}{#a}{#b}{#c}{#d}{>partial:profile name=n count=\"{x}\"/}{/d}{/c}{/b}{/a}{/profile}"
      |> should equal "Hello Mick! You have 30 new messages."

    // should test partial with literal inline param and context. Fallback values for name or count are undefined
    [<Test>]
    let ``should test partial with literal inline param and context; Fallback values for name or count are undefined`` () =
      json "{\"profile\":{\"n\":\"Mick\",\"count\":30}}"
      |> dust  "partial with literal inline param and context"
               "{>partial:profile name=\"Joe\" count=\"99\"/}"
      |> should equal "Hello Joe! You have 30 new messages."

    // should test partial with dynamic name and a context
    [<Test>]
    let ``should test partial with dynamic name and a context`` () =
      json "{\"partialName\":\"partial\",\"me\":{\"name\":\"Mick\",\"count\":30}}"
      |> dust  "partial with dynamic name and context"
               "{>\"{partialName}\":me /}"
      |> should equal "Hello Mick! You have 30 new messages."

    // should test partial with dynamic name and a context
    [<Test>]
    let ``should test partial with dynamic name and a context 2`` () =
      json "{\"partialName\":\"partial\",\"me\":{\"name\":\"Mick\",\"count\":30}}"
      |> dust  "partial with dynamic name and context and inline params"
               "{>\"{partialName}\" name=me.name count=me.count /}"
      |> should equal "Hello Mick! You have 30 new messages."

    // should preserve partials backwards compatibility with compilers pre-2.7
    [<Test>]
    let ``should preserve partials backwards compatibility with compilers pre 2_7`` () =
      json "{\"name\":\"Mick\",\"count\":30}"
      |> dust  "backcompat (< 2.7.0) compiler with no partial context"
               "{#oldPartial/}"
      |> should equal "Hello Mick! You have 30 new messages."

    // should test partial with blocks and inline params
    [<Test>]
    let ``should test partial with blocks and inline params`` () =
      json "{\"n\":\"Mick\",\"c\":30}"
      |> dust  "partial with blocks and inline params"
               "{>partial_with_blocks name=n count=\"{c}\"/}"
      |> should equal "default header Hello Mick! You have 30 new messages."

    // should test partial with blocks, override default values for blocks and inline params
    [<Test>]
    let ``should test partial with blocks, override default values for blocks and inline params`` () =
      json "{\"n\":\"Mick\",\"c\":30}"
      |> dust  "partial with blocks, override default values for blocks and inline params"
               "{>partial_with_blocks name=n count=\"{c}\"/}{<header}my header {/header}"
      |> should equal "my header Hello Mick! You have 30 new messages."

    // should test partial blocks and no defaults, override default values for blocks and inline params
    [<Test>]
    let ``should test partial blocks and no defaults, override default values for blocks and inline params`` () =
      json "{\"n\":\"Mick\",\"c\":30}"
      |> dust  "partial with blocks and no defaults, override default values for blocks and inline params"
               "{>partial_with_blocks_and_no_defaults name=n count=\"{c}\"/}{<header}my header {/header}"
      |> should equal "my header Hello Mick! You have 30 new messages."

    // should test partial with no blocks, ignore the override inline partials
    [<Test>]
    let ``should test partial with no blocks, ignore the override inline partials`` () =
      json "{\"n\":\"Mick\",\"c\":30}"
      |> dust  "partial with no blocks, ignore the override inline partials"
               "{>partial name=n count=\"{c}\"/}{<header}my header {/header}"
      |> should equal "Hello Mick! You have 30 new messages."

    // should print the current template name
    [<Test>]
    let ``should print the current template name`` () =
      json "{}"
      |> dust  "partial prints the current template name"
               "{>partial_print_name/}"
      |> should equal "partial_print_name"

    // should print the current dynamic template name
    [<Test>]
    let ``should print the current dynamic template name`` () =
      json "{\"partial_print_name\":\"partial prints the current template name\"}"
      |> dust  "partial prints the current dynamic template name"
               "{>\"{partial_print_name}\"/}"
      |> should equal "partial_print_name"

    // should print the current template name
    [<Test>]
    let ``should print the current template name 2`` () =
      json "{}"
      |> dust  "nested partial prints the current template name"
               "{>nested_partial_print_name/}"
      |> should equal "partial_print_name"

    // should print the current template name with some additional output
    [<Test>]
    let ``should print the current template name with some additional output`` () =
      json "{\"parentTemplate\":\"parent\",\"parentSource\":\"{?undefinedVar}{:else}{>\\\"content\\\"/}{/undefinedVar}\",\"contentTemplate\":\"content\",\"contentSource\":\"templateName: {#printTemplateName}{/printTemplateName} output: additional output\"}"
      |> dust  "nested partial 2 levels deep from loadSource prints the current template name"
               "{#loadTemplate name=\"{contentTemplate}\" source=\"{contentSource|s}\"}{/loadTemplate}\n{#loadTemplate name=\"{parentTemplate}\" source=\"{parentSource|s}\"}{/loadTemplate}\n{>\"{parentTemplate}\"/} | additional parent output"
      |> should equal "templateName: content output: additional output | additional parent output"

    // should render the helper with missing global context
    [<Test>]
    let ``should render the helper with missing global context`` () =
      json "{}"
      |> dust  "partial with makeBase_missing_global"
               "{#helper template=\"partial\"}{/helper}"
      |> should equal "Hello ! You have  new messages."

    // Should gracefully handle stepping into context that does not exist
    [<Test>]
    let ``Should gracefully handle stepping into context that does not exist`` () =
      json "{}"
      |> dust  "partial stepping into context that does not exist"
               "{#loadPartialTl}{/loadPartialTl}\n{>partialTl:contextDoesNotExist/}"
      |> should equal " "

[<Ignore("TODO implement")>]
module T12_InlineParams =

    // === SUITE ===inline params tests
    // should test inner params
    [<Test>]
    let ``should test inner params`` () =
      json "{}"
      |> dust  "params"
               "{#helper foo=\"bar\"/}"
      |> should equal "bar"

    // Block handlers syntax should support integer number parameters
    [<Test>]
    let ``Block handlers syntax should support integer number parameters`` () =
      json "{}"
      |> dust  "inline params as integer"
               "{#helper foo=10 /}"
      |> should equal "10"

    // Block handlers syntax should support decimal number parameters
    [<Test>]
    let ``Block handlers syntax should support decimal number parameters`` () =
      json "{}"
      |> dust  "inline params as float"
               "{#helper foo=3.14159 /}"
      |> should equal "3.14159"

    // should print negative integer
    [<Test>]
    let ``should print negative integer`` () =
      json "{\"foo\":true}"
      |> dust  "inline params as negative integer"
               "{#foo bar=-1}{bar}{/foo}"
      |> should equal "-1"

    // should print negative float
    [<Test>]
    let ``should print negative float`` () =
      json "{\"foo\":true}"
      |> dust  "inline params as negative float"
               "{#foo bar=-1.1}{bar}{/foo}"
      |> should equal "-1.1"

    // should test parameters with dashes
    [<Test>]
    let ``should test parameters with dashes`` () =
      json "{}"
      |> dust  "inline params with dashes"
               "{#helper data-foo=\"dashes\" /}"
      |> should equal "dashes"

    // Inline params that evaluate to a dust function should evaluate their body
    [<Test>]
    let ``Inline params that evaluate to a dust function should evaluate their body`` () =
      json "{\"section\":true,\"b\":\"world\"}"
      |> dust  "inline params as dust function"
               "{#section a=\"{b}\"}{#a}Hello, {.}!{/a}{/section}"
      |> should equal "Hello, world!"

[<Ignore("TODO implement")>]
module T13_InlinePartialBlock =

    // === SUITE ===inline partial/block tests
    // should test blocks with dynamic keys
    [<Test>]
    let ``should test blocks with dynamic keys`` () =
      json "{\"val\":\"A\"}"
      |> dust  "blocks with dynamic keys"
               "{<title_A}\nAAA\n{/title_A}\n{<title_B}\nBBB\n{/title_B}\n{+\"title_{val}\"/}"
      |> should equal "AAA"

    // should test blocks with more than one dynamic keys
    [<Test>]
    let ``should test blocks with more than one dynamic keys`` () =
      json "{\"val1\":\"title\",\"val2\":\"A\"}"
      |> dust  "blocks with more than one dynamic keys"
               "{<title_A}\nAAA\n{/title_A}\n{<title_B}\nBBB\n{/title_B}\n{+\"{val1}_{val2}\"/}"
      |> should equal "AAA"

    // should test blocks with dynamic key values as objects
    [<Test>]
    let ``should test blocks with dynamic key values as objects`` () =
      json "{\"val1\":\"title\",\"val2\":\"A\",\"obj\":{\"name\":\"B\"}}"
      |> dust  "blocks with dynamic key values as objects"
               "{<title_A}\nAAA\n{/title_A}\n{<title_B}\nBBB\n{/title_B}\n{+\"{val1}_{obj.name}\"/}"
      |> should equal "BBB"

    // should test blocks with dynamic key values as arrays
    [<Test>]
    let ``should test blocks with dynamic key values as arrays`` () =
      json "{\"val1\":\"title\",\"val2\":\"A\",\"obj\":{\"name\":[\"A\",\"B\"]}}"
      |> dust  "blocks with dynamic key values as arrays"
               "{<title_A}\nAAA\n{/title_A}\n{<title_B}\nBBB\n{/title_B}\n{+\"{val1}_{obj.name[0]}\"/}"
      |> should equal "AAA"

[<Ignore("TODO requires JavaScript in context")>]
module T14_Lambda =

    // === SUITE ===lambda tests
    // should test that a non-chunk return value is used for truthiness
    [<Test>]
    let ``should test that a non-chunk return value is used for truthiness`` () =
      json "{\"foo\":{\"foobar\":\"Foo Bar\"}}"
      |> dust  "test that the scope of the function is correct and that a non-chunk return value is used for truthiness checks"
               "Hello {#foo}{#bar}{.}{/bar}{/foo} World!"
      |> should equal "Hello Foo Bar World!"

    // should functions that return false are falsy
    [<Test>]
    let ``should functions that return false are falsy`` () =
      json "{}"
      |> dust  "test that function that do not return chunk and return falsy are treated as falsy"
               "{#bar}{.}{:else}false{/bar}"
      |> should equal "false"

    // should functions that return 0 are truthy
    [<Test>]
    let ``should functions that return 0 are truthy`` () =
      json "{}"
      |> dust  "test that function that do not return chunk and return 0 are treated as truthy (in the Dust sense)"
               "{#bar}{.}{:else}false{/bar}"
      |> should equal "0"

    // should test scope of context function
    [<Test>]
    let ``should test scope of context function`` () =
      json "{\"foo\":{\"foobar\":\"Foo Bar\"}}"
      |> dust  "test that the scope of the function is correct"
               "Hello {#foo}{bar}{/foo} World!"
      |> should equal "Hello Foo Bar World!"

    // should test that function returning object is resolved
    [<Test>]
    let ``should test that function returning object is resolved`` () =
      json "{\"foo\":{\"foobar\":\"Foo Bar\"}}"
      |> dust  "test that function returning object is resolved"
               "Hello {#foo}{bar}{/foo} World!"
      |> should equal "Hello Foo Bar World!"

module R15_CoreGrammar =
    // === SUITE ===core-grammar tests
    // should ignore extra whitespaces between opening brace plus any of (#,?,@,^,+,%) and the tag identifier
    [<Test>]
    let ``should ignore extra whitespaces between opening brace plus any of (#,?,at,^,+,%) and the tag identifier`` () =
      json "{}"
      |> dust  "ignore extra whitespaces between opening brace plus any of (#,?,@,^,+,%) and the tag identifier"
               "{# helper foo=\"bar\" boo=\"boo\" } {/helper}"
      |> should equal "boo bar"

    // should show an error for whitespces between the opening brace and any of (#,?,@,^,+,%)
    [<Test>]
    let ``should show an error for whitespces between the opening brace and any of (#,?,at,^,+,%)`` () =
      json "{}"
      |> dust  "error: whitespaces between the opening brace and any of (#,?,@,^,+,%) is not allowed"
               "{ # helper foo=\"bar\" boo=\"boo\" } {/helper}"
      |> should equal "undefined"

    // should ignore extra whitespaces between the closing brace plus slash and the tag identifier
    [<Test>]
    let ``should ignore extra whitespaces between the closing brace plus slash and the tag identifier`` () =
      json "{}"
      |> dust  "whitespaces between the closing brace plus slash and the tag identifier is supported"
               "{# helper foo=\"bar\" boo=\"boo\"} {/ helper }"
      |> should equal "boo bar"

    // should show an error because whitespaces between the '{' and the forward slash are not allowed in the closing tags
    [<Test>]
    let ``should show an error because whitespaces between the '{' and the forward slash are not allowed in the closing tags`` () =
      json "{}"
      |> dust  "error: whitespaces between the openning curly brace and forward slash in the closing tags not supported"
               "{# helper foo=\"bar\" boo=\"boo\"} { / helper }"
      |> should equal "undefined"

    // should ignore extra whitespaces before the self closing tags
    [<Test>]
    let ``should ignore extra whitespaces before the self closing tags`` () =
      json "{}"
      |> dust  "whitespaces before the self closing tags is allowed"
               "{#helper foo=\"bar\" boo=\"boo\" /}"
      |> should equal "boo bar"

    // should show an error for whitespaces  etween the forward slash and the closing brace in self closing tags
    [<Test>]
    let ``should show an error for whitespaces  etween the forward slash and the closing brace in self closing tags`` () =
      json "{}"
      |> dust  "error: whitespaces between the forward slash and the closing brace in self closing tags"
               "{#helper foo=\"bar\" boo=\"boo\" / }"
      |> should equal "undefined"

    // should ignore extra whitespaces between inline params
    [<Test>]
    let ``should ignore extra whitespaces between inline params`` () =
      json "{}"
      |> dust  "extra whitespaces between inline params supported"
               "{#helper foo=\"bar\"   boo=\"boo\"/}"
      |> should equal "boo bar"

    // should show an error for whitespaces between the '{' plus '>' and partial identifier
    [<Test>]
    let ``should show an error for whitespaces between the '{' plus '>' and partial identifier`` () =
      json "{\"name\":\"Jim\",\"count\":42,\"ref\":\"hello_world\"}"
      |> dust  "error : whitespaces between the \'{\' plus \'>\' and partial identifier is not supported"
               "{ > partial/} {> \"hello_world\"/} {> \"{ref}\"/}"
      |> should equal "undefined"

    // should ignore extra whitespacesbefore the forward slash and the closing brace in partials
    [<Test>]
    let ``should ignore extra whitespacesbefore the forward slash and the closing brace in partials`` () =
      json "{\"name\":\"Jim\",\"count\":42,\"ref\":\"hello_world\"}"
      |> dust  "whitespaces before the forward slash and the closing brace in partials supported"
               "{>partial /} {>\"hello_world\" /} {>\"{ref}\" /}"
      |> should equal "Hello Jim! You have 42 new messages. Hello World! Hello World!"

    // should ignore eol
    [<Test>]
    let ``should ignore eol`` () =
      empty
      |> dust  "ignore whitespaces also means ignoring eol"
               "{#authors \nname=\"theAuthors\"\nlastname=\"authorlastname\" \nmaxtext=300}\n{>\"otherTemplate\"/}\n{/authors}"
      |> should equal ""

    // should ignore carriage return or tab in inline param values
    [<Test>]
    let ``should ignore carriage return or tab in inline param values`` () =
      empty
      |> dust  "ignore carriage return or tab in inline param values"
               "{#helper name=\"Dialog\" config=\"{\n\'name\' : \'index\' }\n \"} {/helper}"
      |> should equal ""

    // should test using dash in key/reference
    [<Test>]
    let ``should test using dash in key/reference`` () =
      json "{\"first-name\":\"Mick\",\"last-name\":\"Jagger\",\"count\":30}"
      |> dust  "support dash in key/reference"
               "Hello {first-name}, {last-name}! You have {count} new messages."
      |> should equal "Hello Mick, Jagger! You have 30 new messages."

    // should test dash in partial's keys
    [<Test>]
    let ``should test dash in partial's keys`` () =
      json "{\"foo-title\":\"title\",\"bar-letter\":\"a\"}"
      |> dust  "support dash in partial\'s key"
               "{<title-a}foo-bar{/title-a}{+\"{foo-title}-{bar-letter}\"/}"
      |> should equal "foo-bar"

    // should test dash in partial's params
    [<Test>]
    let ``should test dash in partial's params`` () =
      json "{\"first-name\":\"Mick\",\"c\":30}"
      |> dust  "support dash in partial\'s params"
               "{>partial name=first-name count=\"{c}\"/}"
      |> should equal "Hello Mick! You have 30 new messages."

    // should test dash in # sections
    [<Test>]
    let ``should test dash in # sections`` () =
      json "{\"first-names\":[{\"name\":\"Moe\"},{\"name\":\"Larry\"},{\"name\":\"Curly\"}]}"
      |> dust  "support dash in # sections"
               "{#first-names}{name}{/first-names}"
      |> should equal "MoeLarryCurly"

    // should test for dash in a referece for exists section
    [<Test>]
    let ``should test for dash in a referece for exists section`` () =
      json "{\"tags-a\":\"tag\"}"
      |> dust  "support dash in a referece for exists section"
               "{?tags-a}tag found!{:else}No Tags!{/tags-a}"
      |> should equal "tag found!"

    // should test base template with dash in the reference
    [<Test>]
    let ``should test base template with dash in the reference`` () =
      empty
      |> dust  "base_template with dash in the reference"
               "Start{~n}{+title-t}Template Title{/title-t}{~n}{+main-t}Template Content{/main-t}{~n}End"
      |> should equal "Start\nTemplate Title\nTemplate Content\nEnd"

    // should test child template with dash
    [<Test>]
    let ``should test child template with dash`` () =
      json "{\"xhr\":false}"
      |> dust  "child_template with dash in the reference"
               "{^xhr-n}tag not found!{:else}tag found!{/xhr-n}"
      |> should equal "tag not found!"

[<Ignore("TODO")>]
module T16_SyntaxError =
    // === SUITE ===syntax error tests
    // should test that the error message shows line and column.
    [<Test>]
    let ``should test that the error message shows line and column`` () =
      json "{\"name\":\"Mick\",\"count\":30}"
      |> dust  "Dust syntax error"
               "RRR {##}"
      |> should equal "undefined"

    // should test the errors message for section with error.
    [<Test>]
    let ``should test the errors message for section with error`` () =
      empty
      |> dust  "Dust syntax error. Error in Section"
               "{#s}\n{#&2}\n{/s}"
      |> should equal "undefined"

    // should test the errors message for section with a buffer and error inside.
    [<Test>]
    let ``should test the errors message for section with a buffer and error inside`` () =
      empty
      |> dust  "Dust syntax error. Error in Section with buffer"
               "{#s}\nthis is the\nbuffer\n{#&2}\na second\nbuffer\n{/s}"
      |> should equal "undefined"

    // should test the errors message for section without end tag shows.
    [<Test>]
    let ``should test the errors message for section without end tag shows`` () =
      empty
      |> dust  "Dust syntax error. Error in Section without end tag"
               "{#s}\nthis is the\nbuffer\na second\nbuffer"
      |> should equal "undefined"

    // should test the errors message for partials with a buffer inside.
    [<Test>]
    let ``should test the errors message for partials with a buffer inside`` () =
      empty
      |> dust  "Dust syntax error. Error in Partial with buffer"
               "{+header}\nthis is a Partial\nwith Error\neeee{@#@$fdf}\ndefault header \n{/header}"
      |> should equal "undefined"

    // should test the errors message for partial without end tag.
    [<Test>]
    let ``should test the errors message for partial without end tag`` () =
      empty
      |> dust  "Dust syntax error. Error in Partial without end tag"
               "{+header}\nthis is the\nbuffer\na second\nbuffer"
      |> should equal "undefined"

    // should test the errors message for Scalar.
    [<Test>]
    let ``should test the errors message for Scalar`` () =
      empty
      |> dust  "Dust syntax error. Error in Scalar"
               "{#scalar}\ntrue\n {#@#fger}\n{:else}\nfalse\n{/scalar}"
      |> should equal "undefined"

    // should test the errors message for Scalar.
    [<Test>]
    let ``should test the errors message for Scalar 2`` () =
      empty
      |> dust  "Dust syntax error. Error in Scalar\'s else"
               "{#scalar}\ntrue\n{:else}\nfalse\n {#@#fger}\n{/scalar}"
      |> should equal "undefined"

    // should test the errors message for Conditionals.
    [<Test>]
    let ``should test the errors message for Conditionals`` () =
      empty
      |> dust  "Dust syntax error. Error in Conditional"
               "{?tags}\n<ul>{~n}\n{#tags}{~s}\n<li>{#@$}</li>{~n}\n{/tags}\n</ul>\n{:else}\nNo Tags!\n{/tags}"
      |> should equal "undefined"

    // should test the errors message for Conditional's else.
    [<Test>]
    let ``should test the errors message for Conditionals else`` () =
      empty
      |> dust  "Dust syntax error. Error in Conditional\'s else"
               "{?tags}\n<ul>{~n}\n{#tags}{~s}\n<li>{.}</li>{~n}\n{/tags}\n</ul>\n{:else}\n{#@$}\nNo Tags!\n{/tags}"
      |> should equal "undefined"

    // should test the errors message for Conditional without end tag.
    [<Test>]
    let ``should test the errors message for Conditional without end tag`` () =
      empty
      |> dust  "Dust syntax error. Error in Conditional without end tag"
               "{?tags}\n<ul>{~n}\n{#tags}{~s}\n<li>{.}</li>{~n}\n{/tags}\n</ul>\n{:else}\nNo Tags!"
      |> should equal "undefined"

    // should test helper syntax errors being handled gracefully
    [<Test>]
    let ``should test helper syntax errors being handled gracefully`` () =
      json "{}"
      |> dust  "Helper syntax error. TypeError"
               "{#hello/}"
      |> should equal "undefined"

    // should test helper syntax errors inside an async block being handled gracefully
    [<Test>]
    let ``should test helper syntax errors inside an async block being handled gracefully`` () =
      json "{}"
      |> dust  "Helper syntax error. async TypeError"
               "{#hello/}"
      |> should equal "undefined"

module R17_Misc =
    // === SUITE ===buffer test
    // given content should be parsed as buffer
    [<Test>]
    let ``given content should be parsed as buffer`` () =
      empty
      |> dust  "buffer "
               "{&partial/}"
      |> should equal "{&partial/}"

    // === SUITE ===comment test
    // comments should be ignored
    [<Test>]
    let ``comments should be ignored`` () =
      empty
      |> dust  "comment"
               "before {!  this is a comment { and } and all sorts of stuff including\nnewlines and tabs     are valid and is simply ignored !}after"
      |> should equal "before after"


module R18_Whitespace =
    // === SUITE ===whitespace test
    // whitespace on: whitespace-only template is preserved
    [<Test>]
    let ``whitespace on: whitespace-only template is preserved`` () =
      empty
      |> dust  "whitespace on: whitespace-only template"
               "\n     "
      |> should equal "\n     "

    // whitespace off: whitespace-only template is removed
    [<Test>]
    let ``whitespace off: whitespace-only template is removed`` () =
      empty
      |> dust  "whitespace off: whitespace-only template"
               "\n     "
      |> should equal ""

    // whitespace on: whitespace-only block is preserved
    [<Test>]
    let ``whitespace on: whitespace-only block is preserved`` () =
      empty
      |> dust  "whitespace on: whitespace-only block"
               "{<foo}\n{/foo}{+foo/}"
      |> should equal "\n"

    // whitespace off: whitespace-only block is removed
    [<Test>]
    let ``whitespace off: whitespace-only block is removed`` () =
      empty
      |> dust  "whitespace off: whitespace-only block"
               "{<foo}\n{/foo}{+foo/}"
      |> should equal ""

    // whitespace off: multiline text block should run together
    [<Test>]
    let ``whitespace off: multiline text block should run together`` () =
      empty
      |> dust  "whitespace off: multiline text block runs together"
               "<p>\n    foo bar baz\n    foo bar baz\n</p>"
      |> should equal "<p>foo bar bazfoo bar baz</p>"

    // whitespace off: multiline text block with a trailing space should not run together
    [<Test>]
    let ``whitespace off: multiline text block with a trailing space should not run together`` () =
      empty
      |> dust  "whitespace off: multiline text block with trailing space does not run together"
               "<p>\n    foo bar baz \n    foo bar baz\n</p>"
      |> should equal "<p>foo bar baz foo bar baz</p>"

    // whitespace on: multiline text block should maintain indent
    [<Test>]
    let ``whitespace on: multiline text block should maintain indent`` () =
      empty
      |> dust  "whitespace on: multiline text block"
               "<p>\n    foo bar baz\n    foo bar baz\n</p>"
      |> should equal "<p>\n    foo bar baz\n    foo bar baz\n</p>"

    // whitespace on: partials should preserve indentation
    [<Test>]
    let ``whitespace on: partials should preserve indentation`` () =
      empty
      |> dust  "whitespace on: partial indentation"
               "<html>\n<head>\n</head>\n<body>{+body/}<body>\n</html>\n{<body}\n    <h1>Title</h1>\n    <p>Content...</p>\n{/body}"
      |> should equal "<html>\n<head>\n</head>\n<body>\n    <h1>Title</h1>\n    <p>Content...</p>\n<body>\n</html>\n"

//[<Ignore("TODO")>]
module T19_RawText =
    // === SUITE ===raw text test
    // raw text should keep all whitespace
    [<Test>]
    [<Ignore "Implement {` .. `}">]
    let ``raw text should keep all whitespace`` () =
      empty
      |> dust  "simple raw text"
               "{`<pre>\nA: \"hello\"\n              B: \'hello\'?\nA: a walrus (:{=\n              B: Lols!\n               __ ___                              \n            .\'. -- . \'.                            \n           /U)  __   (O|                           \n          /.\'  ()()   \'.._                        \n        .\',/;,_.--._.;;) . \'--..__                 \n       /  ,///|.__.|.\\   \'.  \'.\'\'---..___       \n      /\'._ \'\' ||  ||  \'\' _\'  :      \'   . \'.     \n     /        ||  ||        \'.,    )   )   :      \n    :\'-.__ _  ||  ||   _ __.\' __ .\'  \'   \'   ,)   \n    (          \'  |\'        ( __= ___..-._ ( (.\\  \n   (\'      .___ ___.      /\'.___=          ..  \n    \\-..____________..-\'\'                        \n</pre>`}"
      |> should equal "<pre>\nA: \"hello\"\n              B: \'hello\'?\nA: a walrus (:{=\n              B: Lols!\n               __ ___                              \n            .\'. -- . \'.                            \n           /U)  __   (O|                           \n          /.\'  ()()   \'.._                        \n        .\',/;,_.--._.;;) . \'--..__                 \n       /  ,///|.__.|.\\   \'.  \'.\'\'---..___       \n      /\'._ \'\' ||  ||  \'\' _\'  :      \'   . \'.     \n     /        ||  ||        \'.,    )   )   :      \n    :\'-.__ _  ||  ||   _ __.\' __ .\'  \'   \'   ,)   \n    (          \'  |\'        ( __= ___..-._ ( (.\\  \n   (\'      .___ ___.      /\'.___=          ..  \n    \\-..____________..-\'\'                        \n</pre>"

    // raw text is not matching
    [<Test>]
    let ``raw text is not matching`` () =
      json "{\"A\":{\"name\":{\"first\":\"Paul\",\"last\":\"Walrus\"}}}"
      |> dust  "raw text more likely example"
               "{#A}\nbuffer text\n         !spaces and new lines are nullified (by default). Booo\n{~n}   Starting with newline make it not so bad\n{`<pre>\nbut\n  what{\n    we\n      want is this\nhelpful for:\n * talking about Dust syntax which looks like `{ref}` `{@helpers}`\n * interpolations like \'My name is:`} {#name}{first} {last}{/name}{`\n</pre>`}\nafter\n!newline\n{/A}"
      |> should equal "buffer text!spaces and new lines are nullified (by default). Booo\n   Starting with newline make it not so bad<pre>\nbut\n  what{\n      we\n      want is this\nhelpful for:\n * talking about Dust syntax which looks like `{ref}` `{@helpers}`\n * interpolations like \'My name is: Paul Walrus\n</pre>after!newline"

    // raw text should allow {
    [<Test>]
    let ``raw text should allow {`` () =
      empty
      |> dust  "using raw to allow {"
               "<div data-fancy-json={`\"{rawJsonKey: \'value\'}\"`}>\n</div>"
      |> should equal "<div data-fancy-json=\"{rawJsonKey: \'value\'}\"></div>"

[<Ignore("TODO")>]
module T20_Helper =
    // === SUITE ===helper tests
    // helper can return a primitive
    [<Test>]
    let ``helper can return a primitive`` () =
      empty
      |> dust  "helper returns a primitive"
               "{@val value=3/}"
      |> should equal "3"

    // helper can return a primitive and render a body
    [<Test>]
    let ``helper can return a primitive and render a body`` () =
      empty
      |> dust  "helper returns a primitive and renders a body"
               "{@val value=\"world\"}Hello {.}{/val}"
      |> should equal "Hello world"

    // helper that returns an array iterates its body
    [<Test>]
    let ``helper that returns an array iterates its body`` () =
      json "{\"arr\":[{\"name\":\"Alice\"},{\"name\":\"Bob\"},{\"name\":\"Charlie\"}]}"
      |> dust  "helper returns an array and iterates a body"
               "{@val value=arr}Hello {name} {/val}"
      |> should equal "Hello Alice Hello Bob Hello Charlie "

    // helper escapes returned primitives
    [<Test>]
    let ``helper escapes returned primitives`` () =
      empty
      |> dust  "helper escapes a primitive"
               "{@val value=\"You & I\"/}"
      |> should equal "You &amp; I"

    // helper applies filters to returned primitives
    [<Test>]
    let ``helper applies filters to returned primitives`` () =
      empty
      |> dust  "helper filters a primitive"
               "{@val value=\"You & I\" filters=\"|s\"/} {@val value=\"& Tim\" filters=\"|js|s\"/}"
      |> should equal "You & I \"& Tim\""

    // helper filters a primitive using an array of filters
    [<Test>]
    let ``helper filters a primitive using an array of filters`` () =
      json "{\"filters\":[\"js\",\"s\"]}"
      |> dust  "helper filters a primitive using an array of filters"
               "{@val value=\"You & I\" filters=filters/}"
      |> should equal "\"You & I\""

    // helper can return a Chunk
    [<Test>]
    let ``helper can return a Chunk`` () =
      json "{\"hello\":\"<Hello>\"}"
      |> dust  "helper returns a chunk"
               "{@val value=\"{hello} & world\"/}"
      |> should equal "&lt;Hello&gt; & world"

    // helper doesn't apply filters to a Chunk
    [<Test>]
    let ``helper doesn't apply filters to a Chunk`` () =
      json "{\"hello\":\"<Hello>\"}"
      |> dust  "helper doesn\'t filter a chunk"
               "{@val value=\"{hello} & world\" filters=\"|s\"/}"
      |> should equal "&lt;Hello&gt; & world"

    // helper applies filter from esc pragma
    [<Test>]
    let ``helper applies filter from esc pragma`` () =
      empty
      |> dust  "helper filters are affected by pragma"
               "{%esc:s}{@val value=\"You & I\"/}{/esc}"
      |> should equal "You & I"

    // helper filters supercede filter from esc pragma
    [<Test>]
    let ``helper filters supercede filter from esc pragma`` () =
      empty
      |> dust  "helper filters supercede pragma"
               "{%esc:s}{@val value=\"You & I\" filters=\"|h\" /}{/esc}"
      |> should equal "You &amp; I"

    // templates compiled with Dust < 2.7.1 escape values returned from helpers
    [<Test>]
    let ``templates compiled with Dust < 2.7.1 escape values returned from helpers`` () =
      json "{}"
      |> dust  "Dust < 2.7.1 compat: helpers escape references"
               "{#returnLegacy value=\"You & I\" /}"
      |> should equal "You &amp; I"

[<Ignore("TODO")>]
module T21_Debugger =
    // === SUITE ===debugger tests
    // Should crash the application if a helper is not found
    [<Test>]
    let ``Should crash the application if a helper is not found`` () =
      empty
      |> dust  "non-existing helper"
               "some text {@notfound}foo{/notfound} some text"
      |> should equal "undefined"

    // should fail hard for invalid filter
    [<Test>]
    let ``should fail hard for invalid filter`` () =
      json "{\"obj\":\"test\"}"
      |> dust  "invalid filter"
               "{obj|nullcheck|invalid}"
      |> should equal "undefined"

    // test the log messages for a reference not found.
    [<Test>]
    let ``test the log messages for a reference not found.`` () =
      json "{\"test\":\"example text\"}"
      |> dust  "Reference not found"
               "{wrong.test}"
      |> should equal "undefined"

    // test the log messages for an unhandled section.
    [<Test>]
    let ``test the log messages for an unhandled section.`` () =
      json "{\"test\":\"example text\"}"
      |> dust  "Section not found"
               "{#strangeSection}{/strangeSection}"
      |> should equal "undefined"

    // test the log message for an exists block with no body
    [<Test>]
    let ``test the log message for an exists block with no body`` () =
      json "{\"foo\":\"foo\"}"
      |> dust  "Exists without body"
               "{?foo/}"
      |> should equal "undefined"

    // test the log message for a not-exists block with no body
    [<Test>]
    let ``test the log message for a not-exists block with no body`` () =
      empty
      |> dust  "Not exists without body"
               "{^foo/}"
      |> should equal "undefined"

    // test to make sure errors are properly caught and propogated to the callbacks.
    [<Test>]
    let ``test to make sure errors are properly caught and propogated to the callbacks.`` () =
      empty
      |> dust  "Errors should be throwable from helpers and consumed in the render callback/stream onerror"
               "{@error errorMessage=\"helper error\"}{/error}"
      |> should equal "undefined"

