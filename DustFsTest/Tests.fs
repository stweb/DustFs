﻿module Tests

open Dust.Engine
open Dust.Test
open NUnit.Framework
open FsUnit

#if !TODO

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

    // should test nested usage of dotted path resolution
    [<Test>]
    let ``should test nested usage of dotted path resolution`` () =
      json "{\"data\":{\"A\":{\"name\":\"Al\",\"list\":[{\"name\":\"Joe\"},{\"name\":\"Mary\"}],\"B\":{\"name\":\"Bob\",\"Blist\":[\"BB1\"]}}}}"
      |> dust  "nested dotted path resolution"
               "{#data.A.list}{#data.A.B.Blist}{.}Aname{data.A.name}{/data.A.B.Blist}{/data.A.list}"
      |> should equal "BB1AnameAlBB1AnameAl"


    // should test resolve correct 'this' 
    [<Test>]
    let ``should test resolve correct 'this'`` () =
      json "{\"person\":{\"firstName\":\"Peter\",\"lastName\":\"Jones\",\"fullName\":\"Peter Jones\"}}"
      |> dust  "method invocation"
               "Hello {person.fullName}"
      |> should equal "Hello Peter Jones"


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

    // should test usage of dotted path resolution up context
    [<Test>]
    let ``should test usage of dotted path resolution up context 4`` () =
      json "{\"data\":{\"A\":{\"name\":\"Al\",\"B\":\"Ben\",\"C\":{\"namex\":\"Charlie\"}},\"C\":{\"name\":\"Charlie Sr.\"}}}"
      |> dust  "dotted path resolution up context with partial match in current context"
               "{#data}{#A}{C.name}{/A}{/data}"
      |> should equal ""

    // should work when value at end of path is falsey
    [<Test>]
    let ``should work when value at end of path is falsey`` () =
      json "{\"foo\":{\"bar\":0}}"
      |> dust  "Standard dotted path with falsey value. Issue 317"
               "{foo.bar}"
      |> should equal "0"


module R07_NestedPaths =
    // === SUITE ===nested path tests
    // should test the leading dot behavior in local mode
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

module R15_CoreGrammar =
    // === SUITE ===core-grammar tests
    // should ignore extra whitespaces between opening brace plus any of (#,?,@,^,+,%) and the tag identifier
    // should ignore carriage return or tab in inline param values
    [<Test>]
    let ``should ignore carriage return or tab in inline param values`` () =
      empty
      |> dust  "ignore carriage return or tab in inline param values"
               "{#helper name=\"Dialog\" config=\"{\n\'name\' : \'index\' }\n \"} {/helper}"
      |> should equal ""

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

    // should test using dash in key/reference
    [<Test>]
    let ``should test using dash in key/reference`` () =
      json "{\"first-name\":\"Mick\",\"last-name\":\"Jagger\",\"count\":30}"
      |> dust  "support dash in key/reference"
               "Hello {first-name}, {last-name}! You have {count} new messages."
      |> should equal "Hello Mick, Jagger! You have 30 new messages."


module R18_Whitespace =
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

    // raw text should keep all whitespace
    [<Test>]
    let ``raw text should keep all whitespace`` () =
      let out = empty
                |> dust "simple raw text"
                        "{`<pre>\nA: \"hello\"\n              B: \'hello\'?\nA: a walrus (:{=\n              B: Lols!\n               __ ___                              \n            .\'. -- . \'.                            \n           /U)  __   (O|                           \n          /.\'  ()()   \'.._                        \n        .\',/;,_.--._.;;) . \'--..__                 \n       /  ,///|.__.|.\\   \'.  \'.\'\'---..___       \n      /\'._ \'\' ||  ||  \'\' _\'  :      \'   . \'.     \n     /        ||  ||        \'.,    )   )   :      \n    :\'-.__ _  ||  ||   _ __.\' __ .\'  \'   \'   ,)   \n    (          \'  |\'        ( __= ___..-._ ( (.\\  \n   (\'      .___ ___.      /\'.___=          ..  \n    \\-..____________..-\'\'                        \n</pre>`}"

      let exp = "<pre>\nA: \"hello\"\n              B: \'hello\'?\nA: a walrus (:{=\n              B: Lols!\n               __ ___                              \n            .\'. -- . \'.                            \n           /U)  __   (O|                           \n          /.\'  ()()   \'.._                        \n        .\',/;,_.--._.;;) . \'--..__                 \n       /  ,///|.__.|.\\   \'.  \'.\'\'---..___       \n      /\'._ \'\' ||  ||  \'\' _\'  :      \'   . \'.     \n     /        ||  ||        \'.,    )   )   :      \n    :\'-.__ _  ||  ||   _ __.\' __ .\'  \'   \'   ,)   \n    (          \'  |\'        ( __= ___..-._ ( (.\\  \n   (\'      .___ ___.      /\'.___=          ..  \n    \\-..____________..-\'\'                        \n</pre>"
      out |> should equal exp

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


#endif