module Tests

open Dust.Engine
open Dust.Test
open NUnit.Framework
open FsUnit

#if !TODO

//module R01_DustFs =       
//
//    [<Test>]
//    let ``regex should parse tag into name,ctx,kv`` () =
//        let name, ctx, kv = parseInside """test type="primary" name="add_product" label="action.order.now" test2=hallo """
//        name |> expect "test"
//        ctx  |> should equal None
//        kv.TryFindProp "type"  |> shouldBeSome "primary"
//        kv.TryFindProp "name"  |> shouldBeSome "add_product"
//        kv.TryFindProp "label" |> shouldBeSome "action.order.now"
//        kv.TryFindProp "test2" |> shouldBeSome "hallo"
//
//    [<Test>] // {>recursion:./}{
//    let ``regex should parse tag with context`` () =
//        let name, ctx, _ = parseInside """recursion:."""
//        name |> expect "recursion"
//        ctx  |> shouldBeSomeS "." // TODO remove colon...

module R02_CoreTests =
    [<Test>]
    let ``a dot test`` () =
      empty
      |> dust  "dot"
               "{.}"
      |> expect "System.Dynamic.ExpandoObject" // TODO

    [<Test>]
    let ``a dotname test`` () =
      json "{ root: \"ROOT\" }"
      |> dust  "dot"
               "{.root}"
      |> expect "ROOT" // TODO

    [<Test>]
    let ``a dot path with index test`` () =
      empty
      |> dust  "dot"
               "{.C[0]}"
      |> expect ""      

    [<Test>]
    let ``text before and after tags`` () =
      empty
      |> dust  "dot"
               "start\r\n{tag}end"
      |> expect "startend"      

    // javascript-special characters in template names shouldn't break things
    [<Test>]
    let ``javascript-special characters in template names shouldn't break things`` () =
      empty
      |> dust  "confusing \" \n \' \u2028 \u2029 template name\\"
               "Hello World!"
      |> expect "Hello World!"

    // === SUITE ===core tests
    // SKIPPED streaming render
    // should test basic text rendering
    [<Test>]
    let ``should test basic text rendering`` () =
      empty
      |> dustReg  "hello_world"
               "Hello World!"
      |> expect "Hello World!"

    // should test a basic reference
    [<Test>]
    let ``should test a basic reference`` () =
      json "{\"one\":0}"
      |> dust  "reference"
               "{?one}{one}{/one}"
      |> expect "0"

    // should test an implicit array
    [<Test>]
    let ``should test an implicit array`` () =
      json "{\"names\":[\"Moe\",\"Larry\",\"Curly\"]}"
      |> dust  "implicit array"
               "{#names}{.}{~n}{/names}"
      |> expect "Moe\nLarry\nCurly\n"

    // should setup base template for next test. hi should not be part of base block name
    [<Test>]
    let ``should setup base template for next test; hi should not be part of base block name`` () =
      empty
      |> dustReg "issue322"
               "hi{+\"{name}\"/}"
      |> expect "hi"

      empty
      |> dust  "issue322 use base template picks up prefix chunk data"
               "{>issue322 name=\"abc\"/}{<abc}ABC{/abc}"
      |> expect "hiABC"

    // should test base template and overriding blocks "title" and "main"
    [<Test>]
    let ``should test base and child template`` () =
      empty
      |> dustReg  "base_template"
               "Start{~n}{+title}Base Title{/title}{~n}{+main}Base Content{/main}{~n}End"
      |> expect "Start\nBase Title\nBase Content\nEnd"


      json "{\"xhr\":false}"
      |> dust "child_template"
               "{^xhr}{>base_template/}{:else}{+main/}{/xhr}{<title}Child Title{/title}{<main}Child Content{/main}"
      |> expect "Start\nChild Title\nChild Content\nEnd"

    // should test comments (newline between comments is removed)
    [<Test>]
    let ``should test comments`` () =
      empty
      |> dust  "comments"
               "{!\n  Multiline\n  {#foo}{bar}{/foo}\n!}\n{!before!}Hello{!after!}"
      |> expect "Hello"

    // should test escaped characters
    [<Test>]
    let ``should test escaped characters`` () =
      json "{\"safe\":\"<script>alert(\'Hello!\')</script>\",\"unsafe\":\"<script>alert(\'Goodbye!\')</script>\"}"
      |> dust  "filter un-escape"
               "{safe|s}{~n}{unsafe}"
      |> expect "<script>alert(\'Hello!\')</script>\n&lt;script&gt;alert(&#39;Goodbye!&#39;)&lt;/script&gt;"

    // should test force a key
    [<Test>]
    let ``should test force a key`` () =
      json "{\"root\":\"Subject\",\"person\":{\"name\":\"Larry\",\"age\":45}}"
      |> dust  "force local"
               "{#person}{.root}: {name}, {age}{/person}"
      |> expect ": Larry, 45"

    // should render the helper with missing global context
    [<Test>]
    let ``should render the helper with missing global context`` () =
      json "{}"
      |> dust  "makeBase_missing_global"
               "{#helper}{/helper}"
      |> expect ""

    // should ignore eol
    [<Test>]
    let ``should ignore eol`` () =
      empty
      |> dust  "ignore whitespaces also means ignoring eol"
               "{#authors \nname=\"theAuthors\"\nlastname=\"authorlastname\" \nmaxtext=300}\n{>\"otherTemplate\"/}\n{/authors}"
      |> expect ""

    // should test renaming a key
    [<Test>]
    let ``should test renaming a key`` () =
      json "{\"root\":\"Subject\",\"person\":{\"name\":\"Larry\",\"age\":45}}"
      |> dust  "inline param from outer scope"
               "{#person foo=root}{foo}: {name}, {age}{/person}"
      |> expect "Subject: Larry, 45"

    // . creating a block
    [<Test>]
    let ``dot creating a block`` () =
      json "{\"name\":\"me\"}"
      |> dust  "use . for creating a block and set params"
               "{#. test=\"you\"}{name} {test}{/.}"
      |> expect "me you"

module R03_TruthyFalsy =

    // === SUITE ===truth/falsy tests

    // should test for false in the context, evaluated and prints nothing
    [<Test>]
    let ``should test for false in the context, evaluated and prints nothing`` () =
      json "{\"false\":false}"
      |> dust  "false value in context is treated as empty, same as undefined"
               "{false}"
      |> expect ""

    // should test for numeric zero in the context, prints the numeric zero
    [<Test>]
    let ``should test for numeric zero in the context, prints the numeric zero`` () =
      json "{\"zero\":0}"
      |> dust  "numeric 0 value in context is treated as non empty"
               "{zero}"
      |> expect "0"

    // should test emptyString, prints nothing
    [<Test>]
    let ``should test emptyString, prints nothing`` () =
      json "{\"emptyString\":\"\"}"
      |> dust  "empty string context is treated as empty"
               "{emptyString}"
      |> expect ""

    // should test emptyString single quoted, prints nothing
    [<Test>]
    let ``should test emptyString single quoted, prints nothing`` () =
      json "{\"emptyString\":\"\"}"
      |> dust  "empty string, single quoted in context is treated as empty"
               "{emptyString}"
      |> expect ""

    // should test null in the context treated as empty
    [<Test>]
    let ``should test null in the context treated as empty`` () =
      json "{\"NULL\":null}"
      |> dust  "null in the context treated as empty"
               "{NULL}"
      |> expect ""

    // should test undefined in the context treated as empty
    [<Test>]
    let ``should test undefined in the context treated as empty`` () =
      json "{}"
      |> dust  "undefined in the context treated as empty"
               "{UNDEFINED}"
      |> expect ""

    // should test string undefined in the context as non empty
    [<Test>]
    let ``should test string undefined in the context as non empty`` () =
      json "{\"UNDEFINED\":\"undefined\"}"
      |> dust  "undefined string in the context treated as non empty"
               "{UNDEFINED}"
      |> expect "undefined"

    // should test null as empty in exists section
    [<Test>]
    let ``should test null as empty in exists section`` () =
      json "{\"scalar\":null}"
      |> dust  "null is treated as empty in exists"
               "{?scalar}true{:else}false{/scalar}"
      |> expect "false"

    // should test null treated as empty in exists
    [<Test>]
    let ``should test null treated as empty in exists`` () =
      json "{}"
      |> dust  "undefined is treated as empty in exists"
               "{?scalar}true{:else}false{/scalar}"
      |> expect "false"

    // should test null as truthy in not exists
    [<Test>]
    let ``should test null as truthy in not exists`` () =
      json "{\"scalar\":null}"
      |> dust  "null is treated as truthy in not exists"
               "{^scalar}true{:else}false{/scalar}"
      |> expect "true"

    // should test undefined as truthy in not exists
    [<Test>]
    let ``should test undefined as truthy in not exists`` () =
      json "{}"
      |> dust  "undefined is treated as truthy in not exists"
               "{^scalar}true{:else}false{/scalar}"
      |> expect "true"

    // should test null treated as empty in exists
    [<Test>]
    let ``should test undefined treated as empty in exists`` () =
      json "{}"
      |> dust  "undefined is treated as empty in exists"
               "{?scalar}true{:else}false{/scalar}"
      |> expect "false"

module R04_ScalarData =

    // === SUITE ===scalar data tests
    // should test for a scalar null in a # section
    [<Test>]
    let ``should test for a scalar null in a # section`` () =
      json "{\"scalar\":null}"
      |> dust  "scalar null in a # section"
               "{#scalar}true{:else}false{/scalar}"
      |> expect "false"

    // should test for a scalar numeric 0 in a # section
    [<Test>]
    let ``should test for a scalar numeric 0 in a # section`` () =
      json "{\"scalar\":0}"
      |> dust  "scalar numeric 0 in a # section"
               "{#scalar}true{:else}false{/scalar}"
      |> expect "true"

    // should test for a scalar numeric non-zero in a # section
    [<Test>]
    let ``should test for a scalar numeric non-zero in a # section`` () =
      json "{\"scalar\":42}"
      |> dust  "scalar numeric non-zero in a # section"
               "{#scalar}true{:else}false{/scalar}"
      |> expect "true"

    // should test for a scalar string in a # section
    [<Test>]
    let ``should test for a scalar string in a # section`` () =
      json "{\"scalar\":\"abcde\"}"
      |> dust  "scalar non empty string in a # section"
               "{#scalar}true{:else}false{/scalar}"
      |> expect "true"

    // should test for a scalar string in a # section
    [<Test>]
    let ``should test for a scalar string in a # section 2`` () =
      json "{\"scalar\":\"abcde\"}"
      |> dust  "scalar non empty string in a # section"
               "{#scalar}{.}{:else}false{/scalar}"
      |> expect "abcde"

    // should test a missing/undefined scalar value
    [<Test>]
    let ``should test a missing/undefined scalar value`` () =
      json "{\"foo\":0}"
      |> dust  "missing scalar value"
               "{#scalar}true{:else}false{/scalar}"
      |> expect "false"

    // shoud test for scalar true value in the # section
    [<Test>]
    let ``shoud test for scalar true value in the # section`` () =
      json "{\"scalar\":true}"
      |> dust  "scalar true value in the # section"
               "{#scalar}true{:else}false{/scalar}"
      |> expect "true"

    // shoud test for scalar false value in the # section
    [<Test>]
    let ``shoud test for scalar false value in the # section`` () =
      json "{\"scalar\":false}"
      |> dust  "scalar false value in the # section"
               "{#scalar}true{:else}false{/scalar}"
      |> expect "false"

    // should test scalar values true and false are supported in # nor else blocks
    [<Test>]
    let ``should test scalar values true and false are supported in # nor else blocks`` () =
      json "{\"foo\":true,\"bar\":false}"
      |> dust  "scalar values true and false are supported in # nor else blocks "
               "{#foo}foo,{~s}{:else}not foo,{~s}{/foo}{#bar}bar!{:else}not bar!{/bar}"
      |> expect "foo, not bar!"

module R05_EmptyData =

    // === SUITE ===empty data tests
    // empty array is treated as empty in exists
    [<Test>]
    let ``empty array is treated as empty in exists`` () =
      json "{\"array\":[]}"
      |> dust  "empty array is treated as empty in exists"
               "{?array}true{:else}false{/array}"
      |> expect "false"

    // empty {} is treated as non empty in exists
    [<Test>]
    let ``empty {} is treated as non empty in exists`` () =
      json "{\"object\":{}}"
      |> dust  "empty {} is treated as non empty in exists"
               "{?object}true{:else}false{/object}"
      |> expect "true"

    // empty array is treated as empty in a section
    [<Test>]
    let ``empty array is treated as empty in a section`` () =
      json "{\"array\":[]}"
      |> dust  "empty array is treated as empty in a section"
               "{#array}true{:else}false{/array}"
      |> expect "false"

    // empty {} is treated as non empty
    [<Test>]
    let ``empty {} is treated as non empty`` () =
      json "{\"object\":{}}"
      |> dust  "empty {} is treated as non empty in a section"
               "{#object}true{:else}false{/object}"
      |> expect "true"

    // non-empty array in a reference
    [<Test>]
    let ``non-empty array in a reference`` () =
      json "{\"array\":[\"1\",\"2\"]}"
      |> dust  "non-empty array in a reference"
               "{array}"
      |> expect "1,2"

    // should test null string in the context treated as non empty
    [<Test>]
    let ``should test null string in the context treated as non empty`` () =
      json "{\"NULL\":\"null\"}"
      |> dust  "null string in the context treated as non empty"
               "{NULL}"
      |> expect "null"

    // should test for string zero in the context, prints zero
    [<Test>]
    let ``should test for string zero in the context, prints zero`` () =
      json "{\"zero\":\"0\"}"
      |> dust  "string 0 value in context is treated as non empty"
               "{zero}"
      |> expect "0"

    // should test an empty array
    [<Test>]
    let ``should test an empty array`` () =
      json "{\"title\":\"Sir\",\"names\":[]}"
      |> dust  "empty array"
               "{#names}{title} {name}{~n}{/names}"
      |> expect ""

    // should output nothing, but no error should fire
    [<Test>]
    let ``should output nothing, but no error should fire`` () =
      json "{}"
      |> dust  "empty params in helper"
               "{#emptyParamHelper}{/emptyParamHelper}"
      |> expect ""

module R06_Conditional =

    // === SUITE ===conditional tests
    // should test conditional tags
    [<Test>]
    let ``should test conditional tags`` () =
      json "{\"tags\":[],\"likes\":[\"moe\",\"larry\",\"curly\",\"shemp\"]}"
      |> dust  "conditional"
               "{?tags}<ul>{~n}{#tags}{~s} <li>{.}</li>{~n}{/tags}</ul>{:else}No Tags!{/tags}{~n}{^likes}No Likes!{:else}<ul>{~n}{#likes}{~s} <li>{.}</li>{~n}{/likes}</ul>{/likes}"
      |> expect "No Tags!\n<ul>\n  <li>moe</li>\n  <li>larry</li>\n  <li>curly</li>\n  <li>shemp</li>\n</ul>"

    // should test else block when array empty
    [<Test>]
    let ``should test else block when array empty`` () =
      json "{\"foo\":[]}"
      |> dust  "empty else block"
               "{#foo}full foo{:else}empty foo{/foo}"
      |> expect "empty foo"

module R06_ArrayIndexAccess =

    // === SUITE ===array/index-access tests

    // should return a specific array element by index when element value is a primitive
    [<Test>]
    let ``should return a specific array element by index when element value is a primitive`` () =
      json "{\"do\":{\"re\":[\"hello!\",\"bye!\"]}}"
      |> dust  "accessing array element by index when element value is a primitive"
               "{do.re[0]}"
      |> expect "hello!"

    // should return a specific array element by index when element value is a object
    [<Test>]
    let ``should return a specific array element by index when element value is a object`` () =
      json "{\"do\":{\"re\":[{\"mi\":\"hello!\"},\"bye!\"]}}"
      |> dust  "accessing array by index when element value is a object"
               "{do.re[0].mi}"
      |> expect "hello!"

    // should return a specific array element by index when element is a nested object
    [<Test>]
    let ``should return a specific array element by index when element is a nested object`` () =
      json "{\"do\":{\"re\":[{\"mi\":[\"one\",{\"fa\":\"hello!\"}]},\"bye!\"]}}"
      |> dust  "accessing array by index when element is a nested object"
               "{do.re[0].mi[1].fa}"
      |> expect "hello!"

    // should return a specific array element by index when element is list of primitives
    [<Test>]
    let ``should return a specific array element by index when element is list of primitives`` () =
      json "{\"do\":[\"lala\",\"lele\"]}"
      |> dust  "accessing array by index when element is list of primitives"
               "{do[0]}"
      |> expect "lala"

    // should return a specific array element using the current context
    [<Test>]
    let ``should return a specific array element using the current context`` () =
      json "{\"list3\":[[{\"biz\":\"123\"}],[{\"biz\":\"345\"}]]}"
      |> dust  "accessing array inside a loop using the current context"
               "{#list3}{.[0].biz}{/list3}"
      |> expect "123345"

    // test array reference $idx/$len nested loops
    [<Test>]
    let ``test array reference $idx $len nested loops`` () =
      json "{\"A\":[{\"B\":[{\"C\":[\"Ca1\",\"C2\"]},{\"C\":[\"Ca2\",\"Ca22\"]}]},{\"B\":[{\"C\":[\"Cb1\",\"C2\"]},{\"C\":[\"Cb2\",\"Ca2\"]}]}]}"
      |> dust  "array reference $idx/$len nested loops"
               "{#A}A loop:{$idx}-{$len},{#B}B loop:{$idx}-{$len}C[0]={.C[0]} {/B}A loop trailing: {$idx}-{$len}{/A}"
      |> expect "A loop:0-2,B loop:0-2C[0]=Ca1 B loop:1-2C[0]=Ca2 A loop trailing: 0-2A loop:1-2,B loop:0-2C[0]=Cb1 B loop:1-2C[0]=Cb2 A loop trailing: 1-2"

    // should test an array
    [<Test>]
    let ``should test an array`` () =
      json "{\"title\":\"Sir\",\"names\":[{\"name\":\"Moe\"},{\"name\":\"Larry\"},{\"name\":\"Curly\"}]}"
      |> dust  "array"
               "{#names}{title} {name}{~n}{/names}"      
      |> expect "Sir Moe\nSir Larry\nSir Curly\n"

    // array: reference $idx in iteration on objects
    [<Test>]
    let ``array: reference $idx in iteration on objects`` () =
      json "{\"title\":\"Sir\",\"names\":[{\"name\":\"Moe\"},{\"name\":\"Larry\"},{\"name\":\"Curly\"}]}"
      |> dust  "array: reference $idx in iteration on objects"
               "{#names}({$idx}).{title} {name}{~n}{/names}"
      |> expect "(0).Sir Moe\n(1).Sir Larry\n(2).Sir Curly\n"

    // test array: reference $len in iteration on objects
    [<Test>]
    let ``test array: reference $len in iteration on objects`` () =
      json "{\"title\":\"Sir\",\"names\":[{\"name\":\"Moe\"},{\"name\":\"Larry\"},{\"name\":\"Curly\"}]}"
      |> dust  "array: reference $len in iteration on objects"
               "{#names}Size=({$len}).{title} {name}{~n}{/names}"
      |> expect "Size=(3).Sir Moe\nSize=(3).Sir Larry\nSize=(3).Sir Curly\n"

    // test array reference $idx in iteration on simple types
    [<Test>]
    let ``test array reference $idx in iteration on simple types`` () =
      json "{\"title\":\"Sir\",\"names\":[\"Moe\",\"Larry\",\"Curly\"]}"
      |> dust  "array reference $idx in iteration on simple type"
               "{#names}({$idx}).{title} {.}{~n}{/names}"
      |> expect "(0).Sir Moe\n(1).Sir Larry\n(2).Sir Curly\n"

    // test array reference $len in iteration on simple types
    [<Test>]
    let ``test array reference $len in iteration on simple types`` () =
      json "{\"title\":\"Sir\",\"names\":[\"Moe\",\"Larry\",\"Curly\"]}"
      |> dust  "array reference $len in iteration on simple type"
               "{#names}Size=({$len}).{title} {.}{~n}{/names}"
      |> expect "Size=(3).Sir Moe\nSize=(3).Sir Larry\nSize=(3).Sir Curly\n"

    // should HTML-encode stringified arrays referenced directly
    [<Test>]
    let ``should HTML-encode stringified arrays referenced directly`` () =
      json "{\"array\":[\"You & I\",\" & Moe\"]}"
      |> dust  "Outputting an array calls toString and HTML-encodes"
               "{array}"
      |> expect "You &amp; I, &amp; Moe"

    // test array reference $idx/$len on empty array case
    [<Test>]
    let ``test array reference $idx $len on empty array case`` () =
      json "{\"title\":\"Sir\",\"names\":[]}"
      |> dust  "array reference $idx/$len on empty array case"
               "{#names}Idx={$idx} Size=({$len}).{title} {.}{~n}{/names}"
      |> expect ""

    // should test double nested array and . reference: issue #340
    [<Test>]
    let ``should test double nested array and dot reference: issue #340`` () =
      json "{\"test\":[[1,2,3]]}"
      |> dust  "using idx in double nested array"
               "{#test}{#.}{.}i:{$idx}l:{$len},{/.}{/test}"
      |> expect "1i:0l:3,2i:1l:3,3i:2l:3,"
      
    // test array reference $idx/$len on single element case
    [<Test>]
    let ``test array reference $idx $len on single element case`` () =
      json "{\"name\":\"Just one name\"}"
      |> dust  "array reference $idx/$len on single element case (scalar case)"
               "{#name}Idx={$idx} Size={$len} {.}{/name}"
      |> expect "Idx= Size= Just one name"
       
    // test array reference $idx/$len {#.} section case
    [<Test>]
    let ``test array reference $idx $len section case`` () =
      json "{\"names\":[\"Moe\",\"Larry\",\"Curly\"]}"
      |> dust  "array reference $idx/$len {#.} section case"
               "{#names}{#.}{$idx}{.} {/.}{/names}"
      |> expect "0Moe 1Larry 2Curly "

    // test array reference $idx/$len not changed in nested object
    [<Test>]
    let ``test array reference $idx $len not changed in nested object`` () =
      json "{\"results\":[{\"info\":{\"name\":\"Steven\"}},{\"info\":{\"name\":\"Richard\"}}]}"
      |> dust  "array reference $idx/$len not changed in nested object"
               "{#results}{#info}{$idx}{name}-{$len} {/info}{/results}"
      |> expect "0Steven-2 1Richard-2 "
                      
    // Should resolve path correctly
    [<Test>]
    let ``Should resolve index path correctly`` () =
      json "{\"nulls\":[1,null,null,2],\"names\":[{\"name\":\"Moe\"},{\"name\":\"Curly\"}]}"
      |> dust  "check null values in section iteration do not break path resolution"
               "{#nulls}{names[0].name}{/nulls}"
      |> expect "MoeMoeMoeMoe"

module R07_ObjectTests =

    // === SUITE ===object tests
    // should test an object
    [<Test>]
    let ``should test an object`` () =
      json "{\"root\":\"Subject\",\"person\":{\"name\":\"Larry\",\"age\":45}}"
      |> dust  "object"
               "{#person}{root}: {name}, {age}{/person}"
      |> expect "Subject: Larry, 45"

    // should test an object path
    [<Test>]
    let ``should test an object path`` () =
      json "{\"foo\":{\"bar\":\"Hello!\"}}"
      |> dust  "path"
               "{foo.bar}"
      |> expect "Hello!"

    // should test nested usage of dotted path resolution
    [<Test>]
    let ``should test nested usage of dotted path resolution`` () =
      json "{\"data\":{\"A\":{\"name\":\"Al\",\"list\":[{\"name\":\"Joe\"},{\"name\":\"Mary\"}],\"B\":{\"name\":\"Bob\",\"Blist\":[\"BB1\"]}}}}"
      |> dust  "nested dotted path resolution"
               "{#data.A.list}{#data.A.B.Blist}{.}Aname{data.A.name}{/data.A.B.Blist}{/data.A.list}"
      |> expect "BB1AnameAlBB1AnameAl"


    // should test resolve correct 'this' 
    [<Test>]
    let ``should test resolve correct 'this'`` () =
      json "{\"person\":{\"firstName\":\"Peter\",\"lastName\":\"Jones\",\"fullName\":\"Peter Jones\"}}"
      |> dust  "method invocation"
               "Hello {person.fullName}"
      |> expect "Hello Peter Jones"


    // should test usage of dotted path resolution up context
    [<Test>]
    let ``should test usage of dotted path resolution up context`` () =
      json "{\"data\":{\"A\":{\"name\":\"Al\",\"list\":[{\"name\":\"Joe\"},{\"name\":\"Mary\"}],\"B\":{\"name\":\"Bob\",\"Blist\":[\"BB1\",\"BB2\"]}}}}"
      |> dust  "dotted path resolution up context"
               "{#data.A.list}Aname{data.A.name}{/data.A.list}"
      |> expect "AnameAlAnameAl"

    // should test usage of dotted path resolution up context
    [<Test>]
    let ``should test usage of dotted path resolution up context 2`` () =
      json "{\"data\":{\"A\":{\"name\":\"Al\",\"list\":[{\"name\":\"Joe\"},{\"name\":\"Mary\"}],\"B\":{\"name\":\"Bob\",\"Blist\":[\"BB1\",\"BB2\"]}}}}"
      |> dust  "dotted path resolution up context 2"
               "{#data.A.B.Blist}Aname{data.A.name}{/data.A.B.Blist}"
      |> expect "AnameAlAnameAl"

    // should test usage of dotted path resolution up context
    [<Test>]
    let ``should test usage of dotted path resolution up context 3`` () =
      json "{\"data\":{\"A\":{\"name\":\"Al\",\"list\":[{\"name\":\"Joe\"},{\"name\":\"Mary\"}],\"B\":{\"name\":\"Bob\",\"Blist\":[\"BB1\",\"BB2\"]}},\"C\":{\"name\":\"cname\"}}}"
      |> dust  "dotted path resolution without explicit context"
               "{#data.A}Aname{name}{data.C.name}{/data.A}"
      |> expect "AnameAlcname"

    // should test usage of dotted path resolution up context
    [<Test>]
    let ``should test usage of dotted path resolution up context 4`` () =
      json "{\"data\":{\"A\":{\"name\":\"Al\",\"B\":\"Ben\",\"C\":{\"namex\":\"Charlie\"}},\"C\":{\"name\":\"Charlie Sr.\"}}}"
      |> dust  "dotted path resolution up context with partial match in current context"
               "{#data}{#A}{C.name}{/A}{/data}"
      |> expect ""

    // should work when value at end of path is falsey
    [<Test>]
    let ``should work when value at end of path is falsey`` () =
      json "{\"foo\":{\"bar\":0}}"
      |> dust  "Standard dotted path with falsey value. Issue 317"
               "{foo.bar}"
      |> expect "0"

module R07_NestedPaths =
    // === SUITE ===nested path tests
    // should test the leading dot behavior in local mode
    // Should resolve path correctly
    [<Test>]
    let ``Should resolve path correctly 2`` () =
      json "{\"list\":[\"\",2,\"\"],\"a\":{\"b\":\"B\"}}"
      |> dust  "check falsey value in section iteration don\'t break path resolution"
               "{#list}{a.b}{/list}"
      |> expect "BBB"

    // Should resolve path correctly
    [<Test>]
    let ``Should resolve path correctly 3`` () =
      json "{\"list\":[true,2,true],\"a\":{\"b\":\"B\"}}"
      |> dust  "check true value in section iteration are also OK"
               "{#list}{a.b}{/list}"
      |> expect "BBB"

module R10_Partial =

    // === SUITE ===partial definitions
    // should test a basic replace in a template
    // should test dash in partial's params
    [<Test>]
    let ``should test a basic replace in a template and dash in partial's params`` () =
      json "{\"name\":\"Mick\",\"count\":30}"
      |> dustReg "partial"
               "Hello {name}! You have {count} new messages."
      |> expect "Hello Mick! You have 30 new messages."

      json "{\"first-name\":\"Mick\",\"c\":30}"
      |> dust  "support dash in partial\'s params"
               "{>partial name=first-name count=\"{c}\"/}"
      |> expect "Hello Mick! You have 30 new messages."

    // should test a block with defaults
    [<Test>]
    let ``should test a block with defaults`` () =
      json "{\"name\":\"Mick\",\"count\":30}"
      |> dust  "partial_with_blocks"
               "{+header}default header {/header}Hello {name}! You have {count} new messages."
      |> expect "default header Hello Mick! You have 30 new messages."

    // should test a blocks with no defaults
    [<Test>]
    let ``should test a blocks with no defaults`` () =
      json "{\"name\":\"Mick\",\"count\":30}"
      |> dust  "partial_with_blocks_and_no_defaults"
               "{+header/}Hello {name}! You have {count} new messages."
      |> expect "Hello Mick! You have 30 new messages."

    // should test a template with missing helper
    [<Test>]
    let ``should test a template with missing helper`` () =
      empty
      |> dust  "partial_print_name"
               "{#helper}{/helper}"
      |> expect ""

    // should test partial
    [<Test>]
    let ``should test partial`` () =
      empty
      |> dust  "nested_partial_print_name"
               "{>partial_print_name/}"
      |> expect ""

    // should test nested partial
    [<Test>]
    let ``should test nested partial`` () =
      empty
      |> dust  "nested_nested_partial_print_name"
               "{>nested_partial_print_name/}"
      |> expect ""

module R12_InlineParams =

    [<Test>]
    let ``should print negative integer`` () =
      json "{\"foo\":true}"
      |> dust  "inline params as negative integer"
               "{#foo bar=-1}{bar}{/foo}"
      |> expect "-1"

    [<Test>]
    let ``should print negative float`` () =
      json "{\"foo\":true}"
      |> dust  "inline params as negative float"
               "{#foo bar=-1.1}{bar}{/foo}"
      |> expect "-1.1"

module R15_CoreGrammar =
    // === SUITE ===core-grammar tests
    // should ignore extra whitespaces between opening brace plus any of (#,?,@,^,+,%) and the tag identifier
    // should ignore carriage return or tab in inline param values
    [<Test>]
    let ``should ignore carriage return or tab in inline param values`` () =
      empty
      |> dust  "ignore carriage return or tab in inline param values"
               "{#helper name=\"Dialog\" config=\"{\n\'name\' : \'index\' }\n \"} {/helper}"
      |> expect ""

    // should test base template with dash in the reference
    [<Test>]
    let ``should test base template with dash in the reference`` () =
      empty
      |> dust  "base_template with dash in the reference"
               "Start{~n}{+title-t}Template Title{/title-t}{~n}{+main-t}Template Content{/main-t}{~n}End"
      |> expect "Start\nTemplate Title\nTemplate Content\nEnd"

    // should test child template with dash
    [<Test>]
    let ``should test child template with dash`` () =
      json "{\"xhr\":false}"
      |> dust  "child_template with dash in the reference"
               "{^xhr-n}tag not found!{:else}tag found!{/xhr-n}"
      |> expect "tag not found!"

    // should test dash in # sections
    [<Test>]
    let ``should test dash in # sections`` () =
      json "{\"first-names\":[{\"name\":\"Moe\"},{\"name\":\"Larry\"},{\"name\":\"Curly\"}]}"
      |> dust  "support dash in # sections"
               "{#first-names}{name}{/first-names}"
      |> expect "MoeLarryCurly"

    // should test for dash in a referece for exists section
    [<Test>]
    let ``should test for dash in a referece for exists section`` () =
      json "{\"tags-a\":\"tag\"}"
      |> dust  "support dash in a referece for exists section"
               "{?tags-a}tag found!{:else}No Tags!{/tags-a}"
      |> expect "tag found!"

    // should test using dash in key/reference
    [<Test>]
    let ``should test using dash in key/reference`` () =
      json "{\"first-name\":\"Mick\",\"last-name\":\"Jagger\",\"count\":30}"
      |> dust  "support dash in key/reference"
               "Hello {first-name}, {last-name}! You have {count} new messages."
      |> expect "Hello Mick, Jagger! You have 30 new messages."

    // raw text should keep all whitespace
    [<Test>]
    let ``raw text should keep all whitespace`` () =
      let out = empty
                |> dust "simple raw text"
                        "{`<pre>\nA: \"hello\"\n              B: \'hello\'?\nA: a walrus (:{=\n              B: Lols!\n               __ ___                              \n            .\'. -- . \'.                            \n           /U)  __   (O|                           \n          /.\'  ()()   \'.._                        \n        .\',/;,_.--._.;;) . \'--..__                 \n       /  ,///|.__.|.\\   \'.  \'.\'\'---..___       \n      /\'._ \'\' ||  ||  \'\' _\'  :      \'   . \'.     \n     /        ||  ||        \'.,    )   )   :      \n    :\'-.__ _  ||  ||   _ __.\' __ .\'  \'   \'   ,)   \n    (          \'  |\'        ( __= ___..-._ ( (.\\  \n   (\'      .___ ___.      /\'.___=          ..  \n    \\-..____________..-\'\'                        \n</pre>`}"

      let exp = "<pre>\nA: \"hello\"\n              B: \'hello\'?\nA: a walrus (:{=\n              B: Lols!\n               __ ___                              \n            .\'. -- . \'.                            \n           /U)  __   (O|                           \n          /.\'  ()()   \'.._                        \n        .\',/;,_.--._.;;) . \'--..__                 \n       /  ,///|.__.|.\\   \'.  \'.\'\'---..___       \n      /\'._ \'\' ||  ||  \'\' _\'  :      \'   . \'.     \n     /        ||  ||        \'.,    )   )   :      \n    :\'-.__ _  ||  ||   _ __.\' __ .\'  \'   \'   ,)   \n    (          \'  |\'        ( __= ___..-._ ( (.\\  \n   (\'      .___ ___.      /\'.___=          ..  \n    \\-..____________..-\'\'                        \n</pre>"
      out |> expect exp

    // raw text should allow {
    [<Test>]
    let ``raw text should allow {`` () =
      let out = empty|> dust "using raw to allow {"
                             "<div data-fancy-json={`\"{rawJsonKey: \'value\'}\"`}>\n</div>"
      let exp = "<div data-fancy-json=\"{rawJsonKey: \'value\'}\"></div>"
      out |> expect exp



module R17_Misc =
    // === SUITE ===buffer test
    // given content should be parsed as buffer
    [<Test>]
    let ``given content should be parsed as buffer`` () =
      empty
      |> dust  "buffer "
               "{&partial/}"
      |> expect "{&partial/}"

    // === SUITE ===comment test
    // comments should be ignored
    [<Test>]
    let ``comments should be ignored`` () =
      empty
      |> dust  "comment"
               "before {!  this is a comment { and } and all sorts of stuff including\nnewlines and tabs \t are valid and is simply ignored !}after"
      |> expect "before after"

module R18_WhitespaceOff =
    // === SUITE ===whitespace test
    // whitespace off: whitespace-only template is removed
    [<Test>]
    let ``whitespace off: whitespace-only template is removed`` () =
      empty
      |> dust  "whitespace off: whitespace-only template"
               "\n     "
      |> should equal ""

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
      |> expect "<p>foo bar bazfoo bar baz</p>"

    // whitespace off: multiline text block with a trailing space should not run together
    [<Test>]
    let ``whitespace off: multiline text block with a trailing space should not run together`` () =
      empty
      |> dust  "whitespace off: multiline text block with trailing space does not run together"
               "<p>\n    foo bar baz \n    foo bar baz\n</p>"
      |> expect "<p>foo bar baz foo bar baz</p>"

module R19_RawText =
    // === SUITE ===raw text test

    // raw text more likely example
    [<Test>]
    let ``raw text more likely example`` () =
      let out = json "{\"A\":{\"name\":{\"first\":\"Paul\",\"last\":\"Walrus\"}}}"
                |> dust "raw text more likely example"
                        "{#A}\nbuffer text\n         !spaces and new lines are nullified (by default). Booo\n{~n}   Starting with newline make it not so bad\n{`<pre>\nbut\n  what{\n  \twe\n      want is this\nhelpful for:\n * talking about Dust syntax which looks like `{ref}` `{@helpers}`\n * interpolations like \'My name is:`} {#name}{first} {last}{/name}{`\n</pre>`}\nafter\n!newline\n{/A}"
      let exp = "buffer text!spaces and new lines are nullified (by default). Booo\n   Starting with newline make it not so bad<pre>\nbut\n  what{\n  \twe\n      want is this\nhelpful for:\n * talking about Dust syntax which looks like `{ref}` `{@helpers}`\n * interpolations like \'My name is: Paul Walrus\n</pre>after!newline"
      save out exp
      out |> expect exp

#endif