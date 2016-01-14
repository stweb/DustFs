module Tests

open Dust.Engine
open Dust.Test
open NUnit.Framework
open FsUnit

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

    [<Test>] // {>recursion:./}{
    let ``regex should parse tag with context`` () =
        let name, ctx, kv = parseKeyValue """recursion:."""
        name |> should equal "recursion"
        ctx  |> shouldBeSomeS ":." // TODO remove colon...
        
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


    // should test base template and overriding blocks "title" and "main"
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
