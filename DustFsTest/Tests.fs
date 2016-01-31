module Tests

open Dust.Test
open NUnit.Framework

#if !TODO

module R01_DustFs =       
    [<Test>]
    let ``a dot test`` () =
      empty
      |> dust  "{.}"
      |> expect "System.Dynamic.ExpandoObject" // TODO

    [<Test>]
    let ``a dotname test`` () =
      json "{ root: \"ROOT\" }"
      |> dust  "{.root}"
      |> expect "ROOT" // TODO

    [<Test>]
    let ``a dot path with index test`` () =
      empty
      |> dust  "{.C[0]}"
      |> expect ""      

    [<Test>]
    let ``text before and after tags`` () =
      empty
      |> dust  "start\r\n{tag}end"
      |> expect "startend"      

    [<Test>]
    let ``javascript-special characters in template names shouldn't break things`` () =
      empty
      |> dustReg "confusing \" \n \' \u2028 \u2029 template name\\" "Hello World!"
      |> expect "Hello World!"

module R02_CoreTests =

    [<Test>]
    let ``should test basic text rendering`` () =
      empty
      |> dustReg  "hello_world" "Hello World!"
      |> expect "Hello World!"
    
    [<Test>]
    let ``should test a basic reference`` () =
      json "{\"one\":0}"
      |> dust  "{?one}{one}{/one}"
      |> expect "0"

    [<Test>]
    let ``should test an implicit array`` () =
      json "{\"names\":[\"Moe\",\"Larry\",\"Curly\"]}"
      |> dust  "{#names}{.}{~n}{/names}"
      |> expect "Moe\nLarry\nCurly\n"

    [<Test>]
    let ``should setup base template for next test; hi should not be part of base block name`` () =
      empty
      |> dustReg "issue322" "hi{+\"{name}\"/}"
      |> expect "hi"

      empty
      |> dust  "{>issue322 name=\"abc\"/}{<abc}ABC{/abc}"
      |> expect "hiABC"

    [<Test>]
    let ``should test base and child template`` () =
      empty
      |> dustReg  "base_template"
               "Start{~n}{+title}Base Title{/title}{~n}{+main}Base Content{/main}{~n}End"
      |> expect "Start\nBase Title\nBase Content\nEnd"


      json "{\"xhr\":false}"
      |> dustReg "child_template"
               "{^xhr}{>base_template/}{:else}{+main/}{/xhr}{<title}Child Title{/title}{<main}Child Content{/main}"
      |> expect "Start\nChild Title\nChild Content\nEnd"
   
    [<Test>]
    let ``should test comments`` () = // (newline between comments is removed)
      empty
      |> dust  "{!\n  Multiline\n  {#foo}{bar}{/foo}\n!}\n{!before!}Hello{!after!}"
      |> expect "Hello"
   
    [<Test>]
    let ``should test escaped characters`` () =
      json "{\"safe\":\"<script>alert(\'Hello!\')</script>\",\"unsafe\":\"<script>alert(\'Goodbye!\')</script>\"}"
      |> dust  "{safe|s}{~n}{unsafe}"
      |> expect "<script>alert(\'Hello!\')</script>\n&lt;script&gt;alert(&#39;Goodbye!&#39;)&lt;/script&gt;"

    [<Test>]
    let ``should test force a key`` () =
      json "{\"root\":\"Subject\",\"person\":{\"name\":\"Larry\",\"age\":45}}"
      |> dust  "{#person}{.root}: {name}, {age}{/person}"
      |> expect ": Larry, 45"

    [<Test>]
    let ``should render the helper with missing global context`` () =
      json "{}"
      |> dustReg  "makeBase_missing_global"
               "{#helper}{/helper}"
      |> expect ""

    [<Test>]
    let ``should ignore eol`` () =
      empty
      |> dust  "{#authors \nname=\"theAuthors\"\nlastname=\"authorlastname\" \nmaxtext=300}\n{>\"otherTemplate\"/}\n{/authors}"
      |> expect ""

    [<Test>]
    let ``should test renaming a key`` () =
      json "{\"root\":\"Subject\",\"person\":{\"name\":\"Larry\",\"age\":45}}"
      |> dust  "{#person foo=root}{foo}: {name}, {age}{/person}"
      |> expect "Subject: Larry, 45"

    [<Test>]
    let ``dot creating a block`` () =
      json "{\"name\":\"me\"}"
      |> dust  "{#. test=\"you\"}{name} {test}{/.}"
      |> expect "me you"

    [<Test>]    
    [<Ignore "Fix recursion">]
    let ``should test recursion`` () =
      json "{\"name\":\"1\",\"kids\":[{\"name\":\"1.1\",\"kids\":[{\"name\":\"1.1.1\"}]}]}"
      |> dustReg "recursion" "{name}{~n}{#kids}{>recursion:./}{/kids}"        
      |> expect "1\n1.1\n1.1.1\n"

module R03_TruthyFalsy =

    [<Test>]
    let ``should test for false in the context, evaluated and prints nothing`` () =
      json "{\"false\":false}"
      |> dust "{false}"
      |> expect ""

    [<Test>]
    let ``should test for numeric zero in the context, prints the numeric zero`` () =
      json "{\"zero\":0}"
      |> dust "{zero}"
      |> expect "0"

    [<Test>]
    let ``should test emptyString, prints nothing`` () =
      json "{\"emptyString\":\"\"}"
      |> dust "{emptyString}"
      |> expect ""

    [<Test>]
    let ``should test emptyString single quoted, prints nothing`` () =
      json "{\"emptyString\":\"\"}"
      |> dust "{emptyString}"
      |> expect ""

    [<Test>]
    let ``should test null in the context treated as empty`` () =
      json "{\"NULL\":null}"
      |> dust "{NULL}"
      |> expect ""

    [<Test>]
    let ``should test undefined in the context treated as empty`` () =
      json "{}"
      |> dust "{UNDEFINED}"
      |> expect ""

    [<Test>]
    let ``should test string undefined in the context as non empty`` () =
      json "{\"UNDEFINED\":\"undefined\"}"
      |> dust   "{UNDEFINED}"
      |> expect "undefined"

    [<Test>]
    let ``should test null as empty in exists section`` () =
      json "{\"scalar\":null}"
      |> dust   "{?scalar}true{:else}false{/scalar}"
      |> expect "false"

    [<Test>]
    let ``should test null treated as empty in exists`` () =
      json "{}"
      |> dust   "{?scalar}true{:else}false{/scalar}"
      |> expect "false"

    [<Test>]
    let ``should test null as truthy in not exists`` () =
      json "{\"scalar\":null}"
      |> dust   "{^scalar}true{:else}false{/scalar}"
      |> expect "true"

    [<Test>]
    let ``should test undefined as truthy in not exists`` () =
      json "{}"
      |> dust   "{^scalar}true{:else}false{/scalar}"
      |> expect "true"

    [<Test>]
    let ``should test undefined treated as empty in exists`` () =
      json "{}"
      |> dust   "{?scalar}true{:else}false{/scalar}"
      |> expect "false"

module R04_ScalarData =

    // === SUITE ===scalar data tests
    // should test for a scalar null in a # section
    [<Test>]
    let ``should test for a scalar null in a # section`` () =
      json "{\"scalar\":null}"
      |> dust   "{#scalar}true{:else}false{/scalar}"
      |> expect "false"

    [<Test>]
    let ``should test for a scalar numeric 0 in a # section`` () =
      json "{\"scalar\":0}"
      |> dust   "{#scalar}true{:else}false{/scalar}"
      |> expect "true"

    [<Test>]
    let ``should test for a scalar numeric non-zero in a # section`` () =
      json "{\"scalar\":42}"
      |> dust   "{#scalar}true{:else}false{/scalar}"
      |> expect "true"

    [<Test>]
    let ``should test for a scalar string in a # section`` () =
      json "{\"scalar\":\"abcde\"}"
      |> dust   "{#scalar}true{:else}false{/scalar}"
      |> expect "true"

    [<Test>]
    let ``should test for a scalar string in a # section 2`` () =
      json "{\"scalar\":\"abcde\"}"
      |> dust   "{#scalar}{.}{:else}false{/scalar}"
      |> expect "abcde"

    [<Test>]
    let ``should test a missing/undefined scalar value`` () =
      json "{\"foo\":0}"
      |> dust   "{#scalar}true{:else}false{/scalar}"
      |> expect "false"

    [<Test>]
    let ``shoud test for scalar true value in the # section`` () =
      json "{\"scalar\":true}"
      |> dust   "{#scalar}true{:else}false{/scalar}"
      |> expect "true"

    [<Test>]
    let ``shoud test for scalar false value in the # section`` () =
      json "{\"scalar\":false}"
      |> dust   "{#scalar}true{:else}false{/scalar}"
      |> expect "false"

    [<Test>]
    let ``should test scalar values true and false are supported in # nor else blocks`` () =
      json "{\"foo\":true,\"bar\":false}"
      |> dust   "{#foo}foo,{~s}{:else}not foo,{~s}{/foo}{#bar}bar!{:else}not bar!{/bar}"
      |> expect "foo, not bar!"

module R05_EmptyData =

    [<Test>]
    let ``empty array is treated as empty in exists`` () =
      json "{\"array\":[]}"
      |> dust   "{?array}true{:else}false{/array}"
      |> expect "false"

    [<Test>]
    let ``empty {} is treated as non empty in exists`` () =
      json "{\"object\":{}}"
      |> dust   "{?object}true{:else}false{/object}"
      |> expect "true"

    [<Test>]
    let ``empty array is treated as empty in a section`` () =
      json "{\"array\":[]}"
      |> dust   "{#array}true{:else}false{/array}"
      |> expect "false"

    [<Test>]
    let ``empty {} is treated as non empty`` () =
      json "{\"object\":{}}"
      |> dust   "{#object}true{:else}false{/object}"
      |> expect "true"

    [<Test>]
    let ``non-empty array in a reference`` () =
      json "{\"array\":[\"1\",\"2\"]}"
      |> dust   "{array}"
      |> expect "1,2"

    [<Test>]
    let ``should test null string in the context treated as non empty`` () =
      json "{\"NULL\":\"null\"}"
      |> dust   "{NULL}"
      |> expect "null"

    [<Test>]
    let ``should test for string zero in the context, prints zero`` () =
      json "{\"zero\":\"0\"}"
      |> dust   "{zero}"
      |> expect "0"

    [<Test>]
    let ``should test an empty array`` () =
      json "{\"title\":\"Sir\",\"names\":[]}"
      |> dust   "{#names}{title} {name}{~n}{/names}"
      |> expect ""

    [<Test>]
    let ``should output nothing, but no error should fire`` () =
      json "{}"
      |> dust   "{#emptyParamHelper}{/emptyParamHelper}"
      |> expect ""

module R06_Conditional =

    // === SUITE ===conditional tests
    // should test conditional tags
    [<Test>]
    let ``should test conditional tags`` () =
      json "{\"tags\":[],\"likes\":[\"moe\",\"larry\",\"curly\",\"shemp\"]}"
      |> dust   "{?tags}<ul>{~n}{#tags}{~s} <li>{.}</li>{~n}{/tags}</ul>{:else}No Tags!{/tags}{~n}{^likes}No Likes!{:else}<ul>{~n}{#likes}{~s} <li>{.}</li>{~n}{/likes}</ul>{/likes}"
      |> expect "No Tags!\n<ul>\n  <li>moe</li>\n  <li>larry</li>\n  <li>curly</li>\n  <li>shemp</li>\n</ul>"

    [<Test>]
    let ``should test else block when array empty`` () =
      json "{\"foo\":[]}"
      |> dust   "{#foo}full foo{:else}empty foo{/foo}"
      |> expect "empty foo"

module R06_ArrayIndexAccess =

    [<Test>]
    let ``should return a specific array element by index when element value is a primitive`` () =
      json "{\"do\":{\"re\":[\"hello!\",\"bye!\"]}}"
      |> dust   "{do.re[0]}"
      |> expect "hello!"

    [<Test>]
    let ``should return a specific array element by index when element value is a object`` () =
      json "{\"do\":{\"re\":[{\"mi\":\"hello!\"},\"bye!\"]}}"
      |> dust   "{do.re[0].mi}"
      |> expect "hello!"

    [<Test>]
    let ``should return a specific array element by index when element is a nested object`` () =
      json "{\"do\":{\"re\":[{\"mi\":[\"one\",{\"fa\":\"hello!\"}]},\"bye!\"]}}"
      |> dust   "{do.re[0].mi[1].fa}"
      |> expect "hello!"

    [<Test>]
    let ``should return a specific array element by index when element is list of primitives`` () =
      json "{\"do\":[\"lala\",\"lele\"]}"
      |> dust   "{do[0]}"
      |> expect "lala"

    [<Test>]
    let ``should return a specific array element using the current context`` () =
      json "{\"list3\":[[{\"biz\":\"123\"}],[{\"biz\":\"345\"}]]}"
      |> dust   "{#list3}{.[0].biz}{/list3}"
      |> expect "123345"

    [<Test>]
    let ``test array reference $idx $len nested loops`` () =
      json "{\"A\":[{\"B\":[{\"C\":[\"Ca1\",\"C2\"]},{\"C\":[\"Ca2\",\"Ca22\"]}]},{\"B\":[{\"C\":[\"Cb1\",\"C2\"]},{\"C\":[\"Cb2\",\"Ca2\"]}]}]}"
      |> dust   "{#A}A loop:{$idx}-{$len},{#B}B loop:{$idx}-{$len}C[0]={.C[0]} {/B}A loop trailing: {$idx}-{$len}{/A}"
      |> expect "A loop:0-2,B loop:0-2C[0]=Ca1 B loop:1-2C[0]=Ca2 A loop trailing: 0-2A loop:1-2,B loop:0-2C[0]=Cb1 B loop:1-2C[0]=Cb2 A loop trailing: 1-2"

    [<Test>]
    let ``should test an array`` () =
      json "{\"title\":\"Sir\",\"names\":[{\"name\":\"Moe\"},{\"name\":\"Larry\"},{\"name\":\"Curly\"}]}"
      |> dust   "{#names}{title} {name}{~n}{/names}"      
      |> expect "Sir Moe\nSir Larry\nSir Curly\n"

    [<Test>]
    let ``array: reference $idx in iteration on objects`` () =
      json "{\"title\":\"Sir\",\"names\":[{\"name\":\"Moe\"},{\"name\":\"Larry\"},{\"name\":\"Curly\"}]}"
      |> dust   "{#names}({$idx}).{title} {name}{~n}{/names}"
      |> expect "(0).Sir Moe\n(1).Sir Larry\n(2).Sir Curly\n"

    [<Test>]
    let ``test array: reference $len in iteration on objects`` () =
      json "{\"title\":\"Sir\",\"names\":[{\"name\":\"Moe\"},{\"name\":\"Larry\"},{\"name\":\"Curly\"}]}"
      |> dust   "{#names}Size=({$len}).{title} {name}{~n}{/names}"
      |> expect "Size=(3).Sir Moe\nSize=(3).Sir Larry\nSize=(3).Sir Curly\n"

    [<Test>]
    let ``test array reference $idx in iteration on simple types`` () =
      json "{\"title\":\"Sir\",\"names\":[\"Moe\",\"Larry\",\"Curly\"]}"
      |> dust   "{#names}({$idx}).{title} {.}{~n}{/names}"
      |> expect "(0).Sir Moe\n(1).Sir Larry\n(2).Sir Curly\n"

    [<Test>]
    let ``test array reference $len in iteration on simple types`` () =
      json "{\"title\":\"Sir\",\"names\":[\"Moe\",\"Larry\",\"Curly\"]}"
      |> dust   "{#names}Size=({$len}).{title} {.}{~n}{/names}"
      |> expect "Size=(3).Sir Moe\nSize=(3).Sir Larry\nSize=(3).Sir Curly\n"

    [<Test>]
    let ``should HTML-encode stringified arrays referenced directly`` () =
      json "{\"array\":[\"You & I\",\" & Moe\"]}"
      |> dust   "{array}"
      |> expect "You &amp; I, &amp; Moe"

    [<Test>]
    let ``test array reference $idx $len on empty array case`` () =
      json "{\"title\":\"Sir\",\"names\":[]}"
      |> dust   "{#names}Idx={$idx} Size=({$len}).{title} {.}{~n}{/names}"
      |> expect ""

    [<Test>]
    let ``should test using idx in double nested array`` () =
      json "{\"test\":[[1,2,3]]}"
      |> dust   "{#test}{#.}{.}i:{$idx}l:{$len},{/.}{/test}"
      |> expect "1i:0l:3,2i:1l:3,3i:2l:3,"
      
    [<Test>]
    let ``test array reference $idx $len on single element (scalar) case`` () =
      json "{\"name\":\"Just one name\"}"
      |> dust   "{#name}Idx={$idx} Size={$len} {.}{/name}"
      |> expect "Idx= Size= Just one name"
       
    [<Test>]
    let ``test array reference $idx $len section case`` () =
      json "{\"names\":[\"Moe\",\"Larry\",\"Curly\"]}"
      |> dust   "{#names}{#.}{$idx}{.} {/.}{/names}"
      |> expect "0Moe 1Larry 2Curly "

    [<Test>]
    let ``test array reference $idx $len not changed in nested object`` () =
      json "{\"results\":[{\"info\":{\"name\":\"Steven\"}},{\"info\":{\"name\":\"Richard\"}}]}"
      |> dust   "{#results}{#info}{$idx}{name}-{$len} {/info}{/results}"
      |> expect "0Steven-2 1Richard-2 "
                      
    [<Test>]
    let ``Should resolve index path correctly`` () =
      json "{\"nulls\":[1,null,null,2],\"names\":[{\"name\":\"Moe\"},{\"name\":\"Curly\"}]}"
      |> dust   "{#nulls}{names[0].name}{/nulls}"
      |> expect "MoeMoeMoeMoe"

module R07_ObjectTests =

    [<Test>]
    let ``should test an object`` () =
      json "{\"root\":\"Subject\",\"person\":{\"name\":\"Larry\",\"age\":45}}"
      |> dust   "{#person}{root}: {name}, {age}{/person}"
      |> expect "Subject: Larry, 45"

    [<Test>]
    let ``should test an object path`` () =
      json "{\"foo\":{\"bar\":\"Hello!\"}}"
      |> dust   "{foo.bar}"
      |> expect "Hello!"

    [<Test>]
    let ``should test nested usage of dotted path resolution`` () =
      json "{\"data\":{\"A\":{\"name\":\"Al\",\"list\":[{\"name\":\"Joe\"},{\"name\":\"Mary\"}],\"B\":{\"name\":\"Bob\",\"Blist\":[\"BB1\"]}}}}"
      |> dust   "{#data.A.list}{#data.A.B.Blist}{.}Aname{data.A.name}{/data.A.B.Blist}{/data.A.list}"
      |> expect "BB1AnameAlBB1AnameAl"

    [<Test>]
    let ``should test resolve correct 'this'`` () =
      json "{\"person\":{\"firstName\":\"Peter\",\"lastName\":\"Jones\",\"fullName\":\"Peter Jones\"}}"
      |> dust   "Hello {person.fullName}"
      |> expect "Hello Peter Jones"

    [<Test>]
    let ``should test usage of dotted path resolution up context`` () =
      json "{\"data\":{\"A\":{\"name\":\"Al\",\"list\":[{\"name\":\"Joe\"},{\"name\":\"Mary\"}],\"B\":{\"name\":\"Bob\",\"Blist\":[\"BB1\",\"BB2\"]}}}}"
      |> dust   "{#data.A.list}Aname{data.A.name}{/data.A.list}"
      |> expect "AnameAlAnameAl"

    [<Test>]
    let ``should test usage of dotted path resolution up context 2`` () =
      json "{\"data\":{\"A\":{\"name\":\"Al\",\"list\":[{\"name\":\"Joe\"},{\"name\":\"Mary\"}],\"B\":{\"name\":\"Bob\",\"Blist\":[\"BB1\",\"BB2\"]}}}}"
      |> dust   "{#data.A.B.Blist}Aname{data.A.name}{/data.A.B.Blist}"
      |> expect "AnameAlAnameAl"

    [<Test>]
    let ``should test usage of dotted path resolution up context 3`` () =
      json "{\"data\":{\"A\":{\"name\":\"Al\",\"list\":[{\"name\":\"Joe\"},{\"name\":\"Mary\"}],\"B\":{\"name\":\"Bob\",\"Blist\":[\"BB1\",\"BB2\"]}},\"C\":{\"name\":\"cname\"}}}"
      |> dust   "{#data.A}Aname{name}{data.C.name}{/data.A}"
      |> expect "AnameAlcname"

    [<Test>]
    let ``should test usage of dotted path resolution up context 4`` () =
      json "{\"data\":{\"A\":{\"name\":\"Al\",\"B\":\"Ben\",\"C\":{\"namex\":\"Charlie\"}},\"C\":{\"name\":\"Charlie Sr.\"}}}"
      |> dust   "{#data}{#A}{C.name}{/A}{/data}"
      |> expect ""

    [<Test>]
    let ``should work when value at end of path is falsey`` () =
      json "{\"foo\":{\"bar\":0}}"
      |> dust   "{foo.bar}"
      |> expect "0"

module R07_NestedPaths =

    [<Test>]
    let ``Should check falsey value in section iteration does not break path resolution`` () =
      json "{\"list\":[\"\",2,\"\"],\"a\":{\"b\":\"B\"}}"
      |> dust   "{#list}{a.b}{/list}"
      |> expect "BBB"

    [<Test>]
    let ``Should check true value in section iteration are also OK`` () =
      json "{\"list\":[true,2,true],\"a\":{\"b\":\"B\"}}"
      |> dust   "{#list}{a.b}{/list}"
      |> expect "BBB"

module R10_Partial =

    [<Test>]
    let ``should test a basic replace in a template and dash in partial's params`` () =
      json "{\"name\":\"Mick\",\"count\":30}"
      |> dustReg "partial"
                 "Hello {name}! You have {count} new messages."
      |> expect  "Hello Mick! You have 30 new messages."

      json "{\"first-name\":\"Mick\",\"c\":30}"
      |> dust   "{>partial name=first-name count=\"{c}\"/}"
      |> expect "Hello Mick! You have 30 new messages."

    [<Test>]
    let ``should test a block with defaults`` () =
      json "{\"name\":\"Mick\",\"count\":30}"
      |> dustReg "partial_with_blocks"
                 "{+header}default header {/header}Hello {name}! You have {count} new messages."
      |> expect  "default header Hello Mick! You have 30 new messages."

    [<Test>]
    let ``should test a blocks with no defaults`` () =
      json "{\"name\":\"Mick\",\"count\":30}"
      |> dustReg "partial_with_blocks_and_no_defaults"
                 "{+header/}Hello {name}! You have {count} new messages."
      |> expect  "Hello Mick! You have 30 new messages."

    [<Test>]
    let ``should test a template with missing helper`` () =
      empty
      |> dustReg "partial_print_name"
                 "{#helper}{/helper}"
      |> expect  ""

    [<Test>]
    let ``should test partial`` () =
      empty
      |> dustReg "nested_partial_print_name"
                 "{>partial_print_name/}"
      |> expect  ""

    [<Test>]
    let ``should test nested partial`` () =
      empty
      |> dustReg "nested_nested_partial_print_name"
                 "{>nested_partial_print_name/}"
      |> expect  ""

module R12_InlineParams =

    [<Test>]
    let ``should print negative integer with inline params`` () =
      json "{\"foo\":true}"
      |> dust   "{#foo bar=-1}{bar}{/foo}"
      |> expect "-1"

    [<Test>]
    let ``should print negative float with inline params`` () =
      json "{\"foo\":true}"
      |> dust   "{#foo bar=-1.1}{bar}{/foo}"
      |> expect "-1.1"

module R15_CoreGrammar =

    [<Test>]
    let ``should ignore carriage return or tab in inline param values`` () =
      empty
      |> dust   "{#helper name=\"Dialog\" config=\"{\n\'name\' : \'index\' }\n \"} {/helper}"
      |> expect ""

    [<Test>]
    let ``should test base template with dash in the reference`` () =
      empty
      |> dust   "Start{~n}{+title-t}Template Title{/title-t}{~n}{+main-t}Template Content{/main-t}{~n}End"
      |> expect "Start\nTemplate Title\nTemplate Content\nEnd"

    [<Test>]
    let ``should test child template with dash`` () =
      json "{\"xhr\":false}"
      |> dust   "{^xhr-n}tag not found!{:else}tag found!{/xhr-n}"
      |> expect "tag not found!"

    [<Test>]
    let ``should test dash in # sections`` () =
      json "{\"first-names\":[{\"name\":\"Moe\"},{\"name\":\"Larry\"},{\"name\":\"Curly\"}]}"
      |> dust   "{#first-names}{name}{/first-names}"
      |> expect "MoeLarryCurly"

    [<Test>]
    let ``should test for dash in a reference for exists section`` () =
      json "{\"tags-a\":\"tag\"}"
      |> dust   "{?tags-a}tag found!{:else}No Tags!{/tags-a}"
      |> expect "tag found!"

    [<Test>]
    let ``should test using dash in key/reference`` () =
      json "{\"first-name\":\"Mick\",\"last-name\":\"Jagger\",\"count\":30}"
      |> dust   "Hello {first-name}, {last-name}! You have {count} new messages."
      |> expect "Hello Mick, Jagger! You have 30 new messages."

    [<Test>]
    let ``raw text should keep all whitespace`` () =
      empty
      |> dust   "{`<pre>\nA: \"hello\"\n              B: \'hello\'?\nA: a walrus (:{=\n              B: Lols!\n               __ ___                              \n            .\'. -- . \'.                            \n           /U)  __   (O|                           \n          /.\'  ()()   \'.._                        \n        .\',/;,_.--._.;;) . \'--..__                 \n       /  ,///|.__.|.\\   \'.  \'.\'\'---..___       \n      /\'._ \'\' ||  ||  \'\' _\'  :      \'   . \'.     \n     /        ||  ||        \'.,    )   )   :      \n    :\'-.__ _  ||  ||   _ __.\' __ .\'  \'   \'   ,)   \n    (          \'  |\'        ( __= ___..-._ ( (.\\  \n   (\'      .___ ___.      /\'.___=          ..  \n    \\-..____________..-\'\'                        \n</pre>`}"
      |> expect "<pre>\nA: \"hello\"\n              B: \'hello\'?\nA: a walrus (:{=\n              B: Lols!\n               __ ___                              \n            .\'. -- . \'.                            \n           /U)  __   (O|                           \n          /.\'  ()()   \'.._                        \n        .\',/;,_.--._.;;) . \'--..__                 \n       /  ,///|.__.|.\\   \'.  \'.\'\'---..___       \n      /\'._ \'\' ||  ||  \'\' _\'  :      \'   . \'.     \n     /        ||  ||        \'.,    )   )   :      \n    :\'-.__ _  ||  ||   _ __.\' __ .\'  \'   \'   ,)   \n    (          \'  |\'        ( __= ___..-._ ( (.\\  \n   (\'      .___ ___.      /\'.___=          ..  \n    \\-..____________..-\'\'                        \n</pre>"

    [<Test>]
    let ``raw text should allow {`` () =
      empty
      |> dust   "<div data-fancy-json={`\"{rawJsonKey: \'value\'}\"`}>\n</div>"
      |> expect "<div data-fancy-json=\"{rawJsonKey: \'value\'}\"></div>"
      
module R16_SyntaxError =
    
    [<Test>]
    [<ExpectedException>]
    let ``should test that the error message shows line and column`` () =
      json "{\"name\":\"Mick\",\"count\":30}"
      |> dust   "RRR {##}"
      |> ignore

    [<Test>]
    [<ExpectedException>]
    let ``should test the errors message for section with error`` () =
      empty
      |> dust   "{#s}\n{#&2}\n{/s}"
      |> ignore

    [<Test>]
    [<ExpectedException>]
    let ``should test the errors message for section with a buffer and error inside`` () =
      empty
      |> dust   "{#s}\nthis is the\nbuffer\n{#&2}\na second\nbuffer\n{/s}"
      |> ignore

    [<Test>]
    [<ExpectedException>]
    let ``should test the errors message for section without end tag shows`` () =
      empty
      |> dust   "{#s}\nthis is the\nbuffer\na second\nbuffer"
      |> ignore

    [<Test>]
    [<ExpectedException>]
    let ``should test the errors message for partials with a buffer inside`` () =
      empty
      |> dust   "{+header}\nthis is a Partial\nwith Error\neeee{@#@$fdf}\ndefault header \n{/header}"
      |> ignore

    [<Test>]
    [<ExpectedException>]
    let ``should test the errors message for partial without end tag`` () =
      empty
      |> dust   "{+header}\nthis is the\nbuffer\na second\nbuffer"
      |> ignore

    [<Test>]
    [<ExpectedException>]
    let ``should test the errors message for Scalar`` () =
      empty
      |> dust   "{#scalar}\ntrue\n {#@#fger}\n{:else}\nfalse\n{/scalar}"
      |> ignore

    [<Test>]
    [<ExpectedException>]
    let ``should test the errors message for Scalar 2`` () =
      empty
      |> dust   "{#scalar}\ntrue\n{:else}\nfalse\n {#@#fger}\n{/scalar}"
      |> ignore

    [<Test>]
    [<ExpectedException>]
    let ``should test the errors message for Conditionals`` () =
      empty
      |> dust   "{?tags}\n<ul>{~n}\n{#tags}{~s}\n<li>{#@$}</li>{~n}\n{/tags}\n</ul>\n{:else}\nNo Tags!\n{/tags}"
      |> ignore

    [<Test>]
    [<ExpectedException>]
    let ``should test the errors message for Conditionals else`` () =
      empty
      |> dust   "{?tags}\n<ul>{~n}\n{#tags}{~s}\n<li>{.}</li>{~n}\n{/tags}\n</ul>\n{:else}\n{#@$}\nNo Tags!\n{/tags}"
      |> ignore

    [<Test>]
    [<ExpectedException>]
    let ``should test the errors message for Conditional without end tag`` () =
      empty
      |> dust   "{?tags}\n<ul>{~n}\n{#tags}{~s}\n<li>{.}</li>{~n}\n{/tags}\n</ul>\n{:else}\nNo Tags!"
      |> ignore

    [<Test>]
    [<ExpectedException>]
    let ``should test helper syntax errors being handled gracefully`` () =
      empty
      |> dust   "{#hello/}"
      |> ignore

    [<Test>]
    [<ExpectedException>]
    let ``should test helper syntax errors inside an async block being handled gracefully`` () =
      empty
      |> dust   "{#hello/}"
      |> ignore

module R17_Misc =

    [<Test>]
    let ``given content should be parsed as buffer`` () =
      empty
      |> dust   "{&partial/}"
      |> expect "{&partial/}"

    [<Test>]
    let ``comments should be ignored`` () =
      empty
      |> dust   "before {!  this is a comment { and } and all sorts of stuff including\nnewlines and tabs \t are valid and is simply ignored !}after"
      |> expect "before after"

module R18_WhitespaceOff =
    
    [<Test>]
    let ``whitespace off: whitespace-only template is removed`` () =
      empty
      |> dust   "\n     "
      |> expect ""

    [<Test>]
    let ``whitespace off: whitespace-only block is removed`` () =
      empty
      |> dust   "{<foo}\n{/foo}{+foo/}"
      |> expect ""

    [<Test>]
    let ``whitespace off: multiline text block should run together`` () =
      empty
      |> dust   "<p>\n    foo bar baz\n    foo bar baz\n</p>"
      |> expect "<p>foo bar bazfoo bar baz</p>"

    [<Test>]
    let ``whitespace off: multiline text block with a trailing space should not run together`` () =
      empty
      |> dust   "<p>\n    foo bar baz \n    foo bar baz\n</p>"
      |> expect "<p>foo bar baz foo bar baz</p>"

module R19_RawText =

    [<Test>]
    let ``raw text more likely example`` () =
      json "{\"A\":{\"name\":{\"first\":\"Paul\",\"last\":\"Walrus\"}}}"
      |> dust "{#A}\nbuffer text\n         !spaces and new lines are nullified (by default). Booo\n{~n}   Starting with newline make it not so bad\n{`<pre>\nbut\n  what{\n  \twe\n      want is this\nhelpful for:\n * talking about Dust syntax which looks like `{ref}` `{@helpers}`\n * interpolations like \'My name is:`} {#name}{first} {last}{/name}{`\n</pre>`}\nafter\n!newline\n{/A}"
      |> expect "buffer text!spaces and new lines are nullified (by default). Booo\n   Starting with newline make it not so bad<pre>\nbut\n  what{\n  \twe\n      want is this\nhelpful for:\n * talking about Dust syntax which looks like `{ref}` `{@helpers}`\n * interpolations like \'My name is: Paul Walrus\n</pre>after!newline"

#endif
