module Todo

open Dust.Engine
open Dust.Test
open NUnit.Framework

#if !TODO

module T06_ArrayIndexAccess =

    // === SUITE ===array/index-access tests
    // should test the array reference access with idx
    [<Test>]
    let ``should test the array reference access with idx`` () =
      json "{\"list4\":[{\"name\":\"Dog\",\"number\":[1,2,3]},{\"name\":\"Cat\",\"number\":[4,5,6]}]}"
      |> dust   "{#list4} {name} {number[$idx]} {$idx}{/list4}"
      |> expect " Dog 1 0 Cat 5 1"

    [<Test>]
    let ``should test the array reference access with len`` () =
      json "{\"list4\":[{\"name\":\"Dog\",\"number\":[1,2,3]},{\"name\":\"Cat\",\"number\":[4,5,6]}]}"
      |> dust   "{#list4} {name} {number[$len]}{/list4}"
      |> expect " Dog 3 Cat 6"

    [<Test>]
    let ``should test the array reference access with idx and current context`` () =
      json "{\"list3\":[[{\"biz\":\"123\"}],[{\"biz\":\"345\"},{\"biz\":\"456\"}]]}"
      |> dust   "{#list3}{.[$idx].biz}{/list3}"
      |> expect "123456"

    [<Test>]
    let ``should test the array reference access with len and current context`` () =
      json "{\"list3\":[[{\"idx\":\"0\"},{\"idx\":\"1\"},{\"idx\":\"2\"}],[{\"idx\":\"0\"},{\"idx\":\"1\"},{\"idx\":\"2\"}]]}"
      |> dust   "{#list3}{.[$len].idx}{/list3}"
      |> expect "22"

    [<Test>]
    let ``should test using a multilevel reference as a key in array access`` () =
      json "{\"loop\":{\"array\":{\"thing\":{\"sub\":1,\"sap\":2},\"thing2\":\"bar\"}},\"key\":{\"foo\":\"thing\"}}"
      |> dust   "{#loop.array[key.foo].sub}{.}{/loop.array[key.foo].sub}"
      |> expect "1"

module T07_NestedPaths =
    // === SUITE ===nested path tests
    // should test the leading dot behavior in local mode
    [<Test>]
    let ``should test the leading dot behavior in local mode`` () =
      json "{\"name\":\"List of people\",\"age\":\"8 hours\",\"people\":[{\"name\":\"Alice\"},{\"name\":\"Bob\",\"age\":42}]}"
      |> dust   "{#people}{.name} is {?.age}{.age} years old.{:else}not telling us their age.{/age}{/people}"
      |> expect "Alice is not telling us their age.Bob is 42 years old."

    [<Test>] // TODO set cur = true inside explicit # -> how?
    let ``should test explicit context blocks looking further up stack`` () =
      json "{\"data\":{\"A\":{\"name\":\"Al\",\"list\":[{\"name\":\"Joe\"},{\"name\":\"Mary\"}],\"B\":{\"name\":\"Bob\",\"Blist\":[\"BB1\",\"BB2\"]}},\"C\":{\"name\":\"cname\"}}}"
      |> dust   "{#data.A:B}Aname{name}{data.C.name}{/data.A}"
      |> expect "AnameAl"

    [<Test>]
    [<Ignore "TODO Implement global">]
    let ``should test access global despite explicit context`` () =
      json "{\"data\":{\"A\":{\"name\":\"Al\",\"list\":[{\"name\":\"Joe\"},{\"name\":\"Mary\"}],\"B\":{\"name\":\"Bob\",\"Blist\":[\"BB1\",\"BB2\"]}},\"C\":{\"name\":\"cname\"}}}"
      |> dust   "{#data.A:B}Aname{name}{glob.globChild}{/data.A}"
      |> expect "AnameAltestGlobal"

    [<Test>]
    [<Ignore "TODO Implement global">]
    let ``Should check nested ref in global works in global mode`` () =
      empty
      // base: { glob: { globChild: "testGlobal"} },        
      |> dust   "{glob.globChild}"
      |> expect "testGlobal"

    [<Test>]
    [<Ignore "TODO Implement global">]
    let ``Should check nested ref not found in global if partial match`` () =
      json "{\"data\":{\"A\":{\"name\":\"Al\",\"B\":\"Ben\",\"C\":{\"namex\":\"Charlie\"}},\"C\":{\"namey\":\"Charlie Sr.\"}}}"
      |> dust   "{#data}{#A}{C.name}{/A}{/data}"
      |> expect ""

    [<Ignore "Requires JavaScript">]
    [<Test>]
    let ``should test resolve correct 'this' when invoking method`` () =
      json "{\"person\":{\"firstName\":\"Peter\",\"lastName\":\"Jones\", \"fullName\": function() {
                return this.firstName + ' ' + this.lastName;
            }}}"
      |> dust   "Hello {person.fullName}"
      |> expect "Hello Peter Jones"

module T11_PartialParams =

    [<SetUp>]
    let ``setup partials`` () =
      json "{\"name\":\"Mick\",\"count\":30}"
      |> dustReg "partial"
                 "Hello {name}! You have {count} new messages."
      |> expect  "Hello Mick! You have 30 new messages."

      empty
      |> dustReg "hello_world"
                 "Hello World!"
      |> expect  "Hello World!"

    [<Test>]
    let ``should test partials`` () =
      json "{\"name\":\"Jim\",\"count\":42,\"ref\":\"hello_world\"}"
      |> dust   //"partials"
                "{>partial foo=0 /} {>\"hello_world\" foo=1 /} {>\"{ref}\" foo=2 /}"
      |> expect "Hello Jim! You have 42 new messages. Hello World! Hello World!"

    [<Test>]
    [<Ignore("needs async helper")>]
    let ``should test partial with an asynchronously-resolved template name`` () =
      empty
      |> dust   "{>\"{ref}\" /}"
      |> expect "Hello World!"

    [<Test>]
    let ``should test partial with context`` () =
      json "{\"profile\":{\"name\":\"Mick\",\"count\":30}}"
      |> dust   "{>partial:.profile/}"
      |> expect "Hello Mick! You have 30 new messages."

    [<Test>]
    let ``should test partial with blocks, with no default values for blocks`` () =
      json "{\"name\":\"Mick\",\"count\":30}"
      |> dust   "{>partial_with_blocks_and_no_defaults/}"
      |> expect "Hello Mick! You have 30 new messages."

    [<Test>]
    let ``should test partial with blocks, with no default values for blocks, but override default values with inline partials`` () =
      json "{\"name\":\"Mick\",\"count\":30}"
      |> dust   "{>partial_with_blocks_and_no_defaults/}{<header}override header {/header}"
      |> expect "override header Hello Mick! You have 30 new messages."

    [<Test>]
    let ``should test partial with blocks, override default values with inline partials`` () =
      json "{\"name\":\"Mick\",\"count\":30}"
      |> dust   "{>partial_with_blocks/}{<header}my header {/header}"
      |> expect "my header Hello Mick! You have 30 new messages."

    [<Test>]
    let ``should test partial with inline params`` () =
      json "{\"n\":\"Mick\",\"c\":30}"
      |> dust   "{>partial name=n count=\"{c}\"/}"
      |> expect "Hello Mick! You have 30 new messages."

    [<Test>]
    let ``should test partial with inline params tree walk up`` () =
      json "{\"n\":\"Mick\",\"x\":30,\"a\":{\"b\":{\"c\":{\"d\":{\"e\":\"1\"}}}}}"
      |> dust   "{#a}{#b}{#c}{#d}{>partial name=n count=\"{x}\"/}{/d}{/c}{/b}{/a}"
      |> expect "Hello Mick! You have 30 new messages."

    [<Test>]
    let ``should test partial with inline params and context`` () =
      json "{\"profile\":{\"n\":\"Mick\",\"c\":30}}"
      |> dust   "{>partial:profile name=\"{n}\" count=\"{c}\"/}"
      |> expect "Hello Mick! You have 30 new messages."

    [<Test>]
    let ``should test partial with inline params and context tree walk up`` () =
      json "{\"profile\":{\"n\":\"Mick\",\"x\":30,\"a\":{\"b\":{\"c\":{\"d\":{\"e\":\"1\"}}}}}}"
      |> dust   "{#profile}{#a}{#b}{#c}{#d}{>partial:profile name=n count=\"{x}\"/}{/d}{/c}{/b}{/a}{/profile}"
      |> expect "Hello Mick! You have 30 new messages."

    [<Test>]
    let ``should test partial with literal inline param and context; Fallback values for name or count are undefined`` () =
      json "{\"profile\":{\"n\":\"Mick\",\"count\":30}}"
      |> dust   "{>partial:profile name=\"Joe\" count=\"99\"/}"
      |> expect "Hello Joe! You have 30 new messages."

    [<Test>]
    let ``should test partial with dynamic name and a context`` () =
      json "{\"partialName\":\"partial\",\"me\":{\"name\":\"Mick\",\"count\":30}}"
      |> dust   "{>\"{partialName}\":me /}"
      |> expect "Hello Mick! You have 30 new messages."

    [<Test>]
    let ``should test partial with dynamic name and a context 2`` () =
      json "{\"partialName\":\"partial\",\"me\":{\"name\":\"Mick\",\"count\":30}}"
      |> dust   "{>\"{partialName}\" name=me.name count=me.count /}"
      |> expect "Hello Mick! You have 30 new messages."

    // should preserve partials backwards compatibility with compilers pre-2.7
//    [<Test>]
//    [<Ignore("doesn't apply to dustfs")>]
//    let ``should preserve partials backwards compatibility with compilers pre 2_7`` () =
//      json "{\"name\":\"Mick\",\"count\":30}"
//      |> dust  "backcompat (< 2.7.0) compiler with no partial context"
//               "{#oldPartial/}"
//      |> expect "Hello Mick! You have 30 new messages."

    [<Test>]
    let ``should test partial with blocks and inline params`` () =
      json "{\"n\":\"Mick\",\"c\":30}"
      |> dust   "{>partial_with_blocks name=n count=\"{c}\"/}"
      |> expect "default header Hello Mick! You have 30 new messages."

    [<Test>]
    let ``should test partial with blocks, override default values for blocks and inline params`` () =
      json "{\"n\":\"Mick\",\"c\":30}"
      |> dust   "{>partial_with_blocks name=n count=\"{c}\"/}{<header}my header {/header}"
      |> expect "my header Hello Mick! You have 30 new messages."

    [<Test>]
    let ``should test partial blocks and no defaults, override default values for blocks and inline params`` () =
      json "{\"n\":\"Mick\",\"c\":30}"
      |> dust   "{>partial_with_blocks_and_no_defaults name=n count=\"{c}\"/}{<header}my header {/header}"
      |> expect "my header Hello Mick! You have 30 new messages."

    [<Test>]
    let ``should test partial with no blocks, ignore the override inline partials`` () =
      json "{\"n\":\"Mick\",\"c\":30}"
      |> dust   "{>partial name=n count=\"{c}\"/}{<header}my header {/header}"
      |> expect "Hello Mick! You have 30 new messages."

    [<Test>]
    let ``should print the current template name`` () =
      empty
      |> dust   "{>partial_print_name/}"
      |> expect "partial_print_name"

    [<Test>]
    let ``should print the current dynamic template name`` () =
      json "{\"partial_print_name\":\"partial prints the current template name\"}"
      |> dust   "{>\"{partial_print_name}\"/}"
      |> expect "partial_print_name"

    [<Test>]
    let ``should print the current template name 2`` () =
      empty
      |> dust   "{>nested_partial_print_name/}"
      |> expect "partial_print_name"

    [<Test>]
    let ``should print the current template name with some additional output`` () =
      json "{\"parentTemplate\":\"parent\",\"parentSource\":\"{?undefinedVar}{:else}{>\\\"content\\\"/}{/undefinedVar}\",\"contentTemplate\":\"content\",\"contentSource\":\"templateName: {#printTemplateName}{/printTemplateName} output: additional output\"}"
      |> dust   "{#loadTemplate name=\"{contentTemplate}\" source=\"{contentSource|s}\"}{/loadTemplate}\n{#loadTemplate name=\"{parentTemplate}\" source=\"{parentSource|s}\"}{/loadTemplate}\n{>\"{parentTemplate}\"/} | additional parent output"
      |> expect "templateName: content output: additional output | additional parent output"

    [<Test>]
    let ``should render the helper with missing global context`` () =
      empty
      |> dust   "{#helper template=\"partial\"}{/helper}"
      |> expect "Hello ! You have  new messages."

    [<Test>]
    let ``Should gracefully handle stepping into context that does not exist`` () =
      empty
      |> dust   "{#loadPartialTl}{/loadPartialTl}\n{>partialTl:contextDoesNotExist/}"
      |> expect " "

module T12_InlineParams =

    // === SUITE ===inline params tests
    // should test inner params
    [<Test>]
    [<Ignore("requires JS & Block handlers")>]
    let ``should test inner params`` () =
      empty
      // context:  {  helper: function(chunk, context, bodies, params) { return chunk.write(params.foo); } },
      |> dust   "{#helper foo=\"bar\"/}"
      |> expect "bar"

    [<Test>]
    [<Ignore("requires JS & Block handlers")>]
    let ``Block handlers syntax should support integer number parameters`` () =
      empty
      // context:  { helper: function(chunk, context, bodies, params) { return chunk.write(params.foo); } },
      |> dust   "{#helper foo=10 /}"
      |> expect "10"

    [<Test>]
    [<Ignore("requires JS & Block handlers")>]
    let ``Block handlers syntax should support decimal number parameters`` () =
      empty
      // context:  { helper: function(chunk, context, bodies, params) { return chunk.write(params.foo); } },       
      |> dust   "{#helper foo=3.14159 /}"
      |> expect "3.14159"

    [<Test>]
    [<Ignore("requires JS & Block handlers")>]
    let ``should test parameters with dashes`` () =
      empty
      // context:  { helper: function(chunk, context, bodies, params) { return chunk.write(params['data-foo']); } },
      |> dust   "{#helper data-foo=\"dashes\" /}"
      |> expect "dashes"

    [<Test>]
    let ``Inline params that evaluate to a dust function should evaluate their body`` () =
      json "{\"section\":true,\"b\":\"world\"}"
      |> dust   "{#section a=\"{b}\"}{#a}Hello, {.}!{/a}{/section}"
      |> expect "Hello, world!"

module T13_InlinePartialBlock =

    [<Test>]
    let ``should test blocks with dynamic keys`` () =
      json "{\"val\":\"A\"}"
      |> dust   "{<title_A}\nAAA\n{/title_A}\n{<title_B}\nBBB\n{/title_B}\n{+\"title_{val}\"/}"
      |> expect "AAA"

    [<Test>]
    let ``should test blocks with more than one dynamic keys`` () =
      json "{\"val1\":\"title\",\"val2\":\"A\"}"
      |> dust   "{<title_A}\nAAA\n{/title_A}\n{<title_B}\nBBB\n{/title_B}\n{+\"{val1}_{val2}\"/}"
      |> expect "AAA"

    [<Test>]
    let ``should test blocks with dynamic key values as objects`` () =
      json "{\"val1\":\"title\",\"val2\":\"A\",\"obj\":{\"name\":\"B\"}}"
      |> dust   "{<title_A}\nAAA\n{/title_A}\n{<title_B}\nBBB\n{/title_B}\n{+\"{val1}_{obj.name}\"/}"
      |> expect "BBB"

    [<Test>]
    let ``should test blocks with dynamic key values as arrays`` () =
      json "{\"val1\":\"title\",\"val2\":\"A\",\"obj\":{\"name\":[\"A\",\"B\"]}}"
      |> dust   "{<title_A}\nAAA\n{/title_A}\n{<title_B}\nBBB\n{/title_B}\n{+\"{val1}_{obj.name[0]}\"/}"
      |> expect "AAA"

module T15_CoreGrammar =

    [<SetUp>]
    let ``setup helper`` () =
      helpers.["helper"] <- (fun (c:Context) (bodies:BodyDict) (param:KeyValue) (renderBody: unit -> unit) ->
                                match param.TryFind "boo", param.TryFind "foo" with
                                | Some b, Some f -> c.Write b; c.Write " "; c.Write f
                                | _ -> ()
                            )

    [<Test>]
    let ``should ignore extra whitespaces between opening brace plus any of (#,?,at,^,+,%) and the tag identifier`` () =
      empty
      |> dust "{# helper foo=\"bar\" boo=\"boo\" } {/helper}"
      |> expect "boo bar"

    [<Test>]
    [<ExpectedException>]
    let ``should show an error for whitespaces between the opening brace and any of (#,?,at,^,+,%)`` () =
      empty
      |> dust   "{ # helper foo=\"bar\" boo=\"boo\" } {/helper}"
      |> ignore

    [<Test>]
    let ``should ignore extra whitespaces between the closing brace plus slash and the tag identifier`` () =
      empty
      |> dust   "{# helper foo=\"bar\" boo=\"boo\"} {/ helper }"
      |> expect "boo bar"

    [<Test>]
    [<ExpectedException>]
    let ``should show an error because whitespaces between the '{' and the forward slash are not allowed in the closing tags`` () =
      empty
      |> dust   "{# helper foo=\"bar\" boo=\"boo\"} { / helper }"
      |> ignore

    [<Test>]
    let ``should ignore extra whitespaces before the self closing tags`` () =
      empty
      |> dust   "{#helper foo=\"bar\" boo=\"boo\" /}"
      |> expect "boo bar"

    [<Test>]
    [<ExpectedException>]
    let ``should show an error for whitespaces between the forward slash and the closing brace in self closing tags`` () =
      empty
      |> dust   "{#helper foo=\"bar\" boo=\"boo\" / }"
      |> ignore

    [<Test>]
    let ``should ignore extra whitespaces between inline params`` () =
      empty
      |> dust   "{#helper foo=\"bar\"   boo=\"boo\"/}"
      |> expect "boo bar"

    [<Test>]
    [<ExpectedException>]
    let ``should show an error for whitespaces between the '{' plus '>' and partial identifier`` () =
      json "{\"name\":\"Jim\",\"count\":42,\"ref\":\"hello_world\"}"
      |> dust   "{ > partial/} {> \"hello_world\"/} {> \"{ref}\"/}"
      |> ignore

    [<Test>]
    let ``should ignore extra whitespacesbefore the forward slash and the closing brace in partials`` () =
      json "{\"name\":\"Jim\",\"count\":42,\"ref\":\"hello_world\"}"
      |> dust   "{>partial /} {>\"hello_world\" /} {>\"{ref}\" /}"
      |> expect "Hello Jim! You have 42 new messages. Hello World! Hello World!"

    [<Test>]
    let ``should test dash in partial's keys`` () =
      json "{\"foo-title\":\"title\",\"bar-letter\":\"a\"}"
      |> dust   "{<title-a}foo-bar{/title-a}{+\"{foo-title}-{bar-letter}\"/}"
      |> expect "foo-bar"

module T18_WhitespaceOn =
    [<Test>]
    let ``whitespace on: whitespace-only template is preserved`` () =
      empty
      |> dust   "\n     "
      |> expect "\n     "

    [<Test>]
    let ``whitespace on: whitespace-only block is preserved`` () =
      empty
      |> dust   "{<foo}\n{/foo}{+foo/}"
      |> expect "\n"    
      
    [<Test>]
    let ``whitespace on: multiline text block should maintain indent`` () =
      empty
      |> dust   "<p>\n    foo bar baz\n    foo bar baz\n</p>"
      |> expect "<p>\n    foo bar baz\n    foo bar baz\n</p>"

    [<Test>]
    let ``whitespace on: partials should preserve indentation`` () =
      empty
      |> dust   "<html>\n<head>\n</head>\n<body>{+body/}<body>\n</html>\n{<body}\n    <h1>Title</h1>\n    <p>Content...</p>\n{/body}"
      |> expect "<html>\n<head>\n</head>\n<body>\n    <h1>Title</h1>\n    <p>Content...</p>\n<body>\n</html>\n"

module T20_Helper =
    [<Test>]
    let ``helper can return a primitive`` () =
      empty
      |> dust   "{@val value=3/}"
      |> expect "3"

    [<Test>]
    let ``helper can return a primitive and render a body`` () =
      empty
      |> dust   "{@val value=\"world\"}Hello {.}{/val}"
      |> expect "Hello world"

    [<Test>]
    let ``helper that returns an array iterates its body`` () =
      json "{\"arr\":[{\"name\":\"Alice\"},{\"name\":\"Bob\"},{\"name\":\"Charlie\"}]}"
      |> dust   "{@val value=arr}Hello {name} {/val}"
      |> expect "Hello Alice Hello Bob Hello Charlie "

    [<Test>]
    let ``helper escapes returned primitives`` () =
      empty
      |> dust   "{@val value=\"You & I\"/}"
      |> expect "You &amp; I"

    [<Test>]
    let ``helper applies filters to returned primitives`` () =
      empty
      |> dust   "{@val value=\"You & I\" filters=\"|s\"/} {@val value=\"& Tim\" filters=\"|js|s\"/}"
      |> expect "You & I \"& Tim\""

    [<Test>]
    let ``helper filters a primitive using an array of filters`` () =
      json "{\"filters\":[\"js\",\"s\"]}"
      |> dust   "{@val value=\"You & I\" filters=filters/}"
      |> expect "\"You & I\""

    [<Test>]
    let ``helper can return a Chunk`` () =
      json "{\"hello\":\"<Hello>\"}"
      |> dust   "{@val value=\"{hello} & world\"/}"
      |> expect "&lt;Hello&gt; & world"

    [<Test>]
    let ``helper doesn't apply filters to a Chunk`` () =
      json "{\"hello\":\"<Hello>\"}"
      |> dust   "{@val value=\"{hello} & world\" filters=\"|s\"/}"
      |> expect "&lt;Hello&gt; & world"

    [<Test>]
    let ``helper applies filter from esc pragma`` () =
      empty
      |> dust   "{%esc:s}{@val value=\"You & I\"/}{/esc}"
      |> expect "You & I"

    [<Test>]
    let ``helper filters supercede filter from esc pragma`` () =
      empty
      |> dust   "{%esc:s}{@val value=\"You & I\" filters=\"|h\" /}{/esc}"
      |> expect "You &amp; I"

    [<Test>]
    let ``templates compiled with Dust < 2.7.1 escape values returned from helpers`` () =
      empty
      |> dust   "{#returnLegacy value=\"You & I\" /}"
      |> expect "You &amp; I"

#endif
#if TODO

[<Ignore "requires JS helper">]
module T02_CoreJSTests =
    [<Test>]
    let ``should test escape pragma`` () =
      json "{\"unsafe\":\"<script>alert(\'Goodbye!\')</script>\"}"
      |> dust   "{%esc:s}\n  {unsafe}{~n}\n  {%esc:h}\n    {unsafe}\n  {/esc}\n{/esc}"
      |> expect "<script>alert(\'Goodbye!\')</script>\n&lt;script&gt;alert(&#39;Goodbye!&#39;)&lt;/script&gt;"

    [<Test>]
    [<Ignore "requires JS helper">]
    let ``should render the template name`` () =
      empty
      |> dust   "{#helper foo=\"bar\" boo=\"boo\"} {/helper}"
      |> expect "global_template"

    [<Test>]
    [<Ignore "requires JS helper">]   
    let ``should render the template name with paths`` () =
      empty
      |> dust   "{#helper foo=\"bar\" boo=\"boo\" template=\"tl/apps/test\"} {/helper}"
      |> expect "apps/test/foo.tl&v=0.1"

    [<Test>]
    [<Ignore "requires JS in context">]
    let ``should functions in context`` () =
      empty
      |> dust   "Hello {type} World!"
      |> expect "Hello Sync World!"

    [<Test>]
    [<Ignore "requires JS in context">]
    let ``should test functions in context`` () =
      empty
      |> dust   "Hello {type} World!"
      |> expect "Hello Async World!"

    [<Test>]
    [<Ignore "requires JS in context">]
    let ``should test sync chunk write`` () =
      empty
      |> dust   "Hello {type} World!"
      |> expect "Hello Chunky World!"

    [<Test>]
    [<Ignore "Requires JS Context">]
    let ``context resolve() taps parameters from the context`` () =
      json "{\"baz\":\"baz\",\"ref\":\"ref\"}"
      |> dust   "{#foo bar=\"{baz} is baz \" literal=\"literal \" func=func chunkFunc=\"{chunkFunc}\" indirectChunkFunc=indirectChunkFunc ref=ref }Fail{/foo}"
      |> expect "baz is baz literal func chunk indirect ref"

    [<Test>]
    [<Ignore "Requires JS Context">]
    let ``should test the context`` () =
      json "{\"projects\":[{\"name\":\"Mayhem\"},{\"name\":\"Flash\"},{\"name\":\"Thunder\"}]}"
      |> dust   "{#list:projects}{name}{:else}No Projects!{/list}"
      |> expect "<ul>\n<li>Mayhem</li>\n<li>Flash</li>\n<li>Thunder</li>\n</ul>"

    [<Test>]
    [<Ignore "Requires JS Context">]
    let ``should allow pushing and popping a context`` () =
      empty
      |> dust   "{#helper}{greeting} {firstName} {lastName}{.}{/helper}"
      |> expect "Hello Dusty Dusterson!"

    [<Test>]
    [<Ignore "Requires JS Context">]
    let ``should allow cloning a context`` () =
      empty
      |> dust   "{#helper}{greeting} {firstName} {lastName}{/helper}"
      |> expect "Hello Dusty Dusterson"


[<Ignore "Implement thenable/promises">]
module Z07_ObjectTestsWithThenable =

    [<Test>]
    let ``should reserve an async chunk for a thenable reference`` () =
      json "{\"magic\":{}}"
      |> dust   "Eventually {magic}!"
      |> expect "Eventually magic!"

    [<Test>]
    let ``undefined`` () =
      json "{\"rice-krispies\":{}}"
      |> dust   "{rice-krispies} {rice-krispies|s}"
      |> expect "Snap, Crackle &amp; Pop Snap, Crackle & Pop"

    [<Test>]
    let ``should deep-inspect a thenable reference`` () =
      json "{\"magic\":{}}"
      |> dust  "Eventually {magic.ally.delicious}!"
      |> expect "Eventually Lucky Charms!"

    [<Test>]
    let ``should deep-inspect a thenable reference but move on if it isn't there`` () =
      json "{\"magic\":{}}"
      |> dust   "Eventually {magic.ally.disappeared}!"
      |> expect "Eventually !"

    [<Test>]
    let ``should deep-inspect a thenable reference recursively`` () =
      json "{\"magic\":{}}"
      |> dust   "Eventually {magic.ally.delicious}!"
      |> expect "Eventually Lucky Charms!"

    [<Test>]
    let ``should inspect a thenable reference but move on if it fails`` () =
      json "{\"magic\":{}}"
      |> dust   "Eventually {magic.ally.delicious}!"
      |> expect "Eventually !"

    [<Test>]
    let ``should deep-inspect a thenable reference but move on if it fails`` () =
      json "{\"magic\":{}}"
      |> dust   "Eventually {magic.ally.delicious}!"
      |> expect "Eventually !"

    [<Test>]
    let ``should reserve an async section for a thenable`` () =
      json "{\"promise\":{}}"
      |> dust   "{#promise}Eventually {magic}!{/promise}"
      |> expect "Eventually magic!"

    [<Test>]
    let ``should iterate over an array resolved from a thenable`` () =
      json "{\"promise\":{}}"
      |> dust   "{promise}"
      |> expect "foo,bar,baz"

    [<Test>]
    let ``should iterate over an array resolved from a thenable 2`` () =
      json "{\"promise\":{}}"
      |> dust   "{#promise}{name}{/promise}"
      |> expect "foobarbaz"

    [<Test>]
    let ``Should not render a thenable section with no body`` () =
      json "{\"promise\":{}}"
      |> dust   "{#promise/}"
      |> expect ""

    [<Test>]
    let ``should reserve an async section for a thenable returned from a function`` () =
      empty
      |> dust   "{#promise}Eventually {magic}!{/promise}"
      |> expect "Eventually magic!"

    [<Test>]
    let ``should reserve an async section for a deep-reference thenable`` () =
      json "{\"magic\":{}}"
      |> dust   "Eventually my {#magic.ally}{delicious}{/magic.ally} will come"
      |> expect "Eventually my Lucky Charms will come"

    [<Test>]
    let ``should reserve an async section for a deep-reference thenable and not blow the stack`` () =
      json "{\"prince\":\"Prince\",\"magic\":{}}"
      |> dust   "Eventually my {#magic.ally}{prince} {delicious} {charms}{/magic.ally} will come"
      |> expect "Eventually my Prince Lucky Charms will come"

    [<Test>]
    let ``Dust helpers that return thenables are resolved in context`` () =
      empty
      |> dust   "{@promise resolve=\"helper\"}I am a big {.}!{/promise}"
      |> expect "I am a big helper!"

    [<Test>]
    let ``Dust helpers that return thenables are rejected in context`` () =
      empty
      |> dust  "{@promise reject=\"error\"}I am a big helper!{:error}I am a big {.}!{/promise}"
      |> expect "I am a big error!"

    [<Test>]
    let ``rejected thenable reference logs`` () =
      json "{\"promise\":{}}"
      |> dust   "{promise}"
      |> expect "undefined"

    [<Test>]
    let ``rejected thenable renders error block`` () =
      json "{\"promise\":{}}"
      |> dust   "{#promise}No magic{:error}{message}{/promise}"
      |> expect "promise error"

[<Ignore "Implement streams">]
module Z07_ObjectTestsWithStreams =

    [<Test>]
    let ``should reserve an async chunk for a stream reference`` () =
      empty
      |> dust   "Stream of {stream}..."
      |> expect "Stream of consciousness..."

    [<Test>]
    let ``should respect filters set on stream references`` () =
      empty
      |> dust   "{polluted|s} {polluted}"
      |> expect "<&> &lt;&amp;&gt;"

    [<Test>]
    let ``should abort the stream if it raises an error`` () =
      empty
      |> dust   "{stream}..."
      |> expect "Everything is..."

    [<Test>]
    let ``should reserve an async section for a stream`` () =
      empty
      |> dust   "Pour {#molecule}{atom}{num}{/molecule} in the glass"
      |> expect "Pour H2O in the glass"

    [<Test>]
    let ``should reserve an async chunk for a stream reference and abort if the stream errors`` () =
      empty
      |> dust   "Pour {#molecule}{atom}{num}{:error}{message}{/molecule} in the glass"
      |> expect "Pour H2O... no! in the glass"

    [<Test>]
    let ``should render streams found while iterating over an array`` () =
      json "{\"streams\":[null,null,null]}"
      |> dust   "{#streams}{.} {/streams}"
      |> expect "Danube Rhine Seine "

    [<Test>]
    let ``should seamlessly mix asynchronous data sources`` () =
      json "{\"water\":{}}"
      |> dust   "Little Bobby drank and drank, and then he drank some more. But what he thought was {water} was {sulfuric_acid}!"
      |> expect "Little Bobby drank and drank, and then he drank some more. But what he thought was H2O was H2SO4!"

    [<Test>]
    let ``should not treat MongoDB documents as streams`` () =
      json "{\"mongo\":{\"name\":\"Mongo\"}}"
      |> dust   "{#mongo}{name}{/mongo}"
      |> expect "Mongo"

    [<Test>]
    let ``stream section with no body should not render`` () =
      empty
      |> dust   "{#stream/}"
      |> expect ""

[<Ignore "TODO implement j filters">]
module Z09_Filter =

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
      |> dust   "{#filter}foo {bar}{/filter}"
      |> expect "FOO BAR"

    [<Test>]
    let ``should escapeJs a string when using the j filter`` () =
      json "{\"obj\":\"<script>\\\\testBS\\\\ \\rtestCR\\r \u2028testLS\u2028 \u2029testPS\u2029 \\ntestNL\\n \\ftestLF\\f \'testSQ\' \\ttestTB\\t /testFS/</script>\"}"
      |> dust   "{obj|j|s}"
      |> expect "<script>\\\\testBS\\\\ \\rtestCR\\r \\u2028testLS\\u2028 \\u2029testPS\\u2029 \\ntestNL\\n \\ftestLF\\f \\\'testSQ\\\' \\ttestTB\\t \\/testFS\\/<\\/script>"

    [<Test>]
    let ``should escapeJs a string with double quotes when using the j filter`` () =
      json "{\"obj\":\"\\\"testDQ\\\"\"}"
      |> dust   "{obj|j|s}"
      |> expect "\\\"testDQ\\\""

    [<Test>]
    let ``should stringify a JSON literal when using the js filter`` () =
      json "{\"obj\":{\"id\":1,\"name\":\"bob\",\"occupation\":\"construction\"}}"
      |> dust   "{obj|js|s}"
      |> expect "{\"id\":1,\"name\":\"bob\",\"occupation\":\"construction\"}"

    [<Test>]
    let ``should escape bad characters when using the js filter`` () =
      json "{\"obj\":{\"name\":\"<<\u2028testLS \u2029testPS\"}}"
      |> dust   "{obj|js|s}"
      |> expect "{\"name\":\"\\u003c\\u003c\\u2028testLS \\u2029testPS\"}"

    [<Test>]
    let ``should objectify a JSON string when using the jp filter`` () =
      json "{\"obj\":\"{\\\"id\\\":1,\\\"name\\\":\\\"bob\\\",\\\"occupation\\\":\\\"construction\\\"}\"}"
      |> dust   "{obj|jp}"
      |> expect "[object Object]"

    [<Test>]
    let ``filters are passed the current context`` () =
      json "{\"woo\":0,\"name\":\"Boo\",\"dust\":{\"woo\":5,\"name\":\"Dust\"}}"
      |> dust   "{#dust}{name|woo}{/dust}"
      |> expect "DUST!!!!!"

[<Ignore("TODO requires JavaScript in context")>]
module Z14_Lambda =

    [<Test>]
    let ``should test that a non-chunk return value is used for truthiness`` () =
      json "{\"foo\":{\"foobar\":\"Foo Bar\"}}"
      |> dust   "Hello {#foo}{#bar}{.}{/bar}{/foo} World!"
      |> expect "Hello Foo Bar World!"

    [<Test>]
    let ``should test functions that return false are falsy`` () =
      empty
      |> dust   "{#bar}{.}{:else}false{/bar}"
      |> expect "false"

    [<Test>]
    let ``should test functions that return 0 are truthy`` () =
      empty
      |> dust   "{#bar}{.}{:else}false{/bar}"
      |> expect "0"

    [<Test>]
    let ``should test scope of context function`` () =
      json "{\"foo\":{\"foobar\":\"Foo Bar\"}}"
      |> dust   "Hello {#foo}{bar}{/foo} World!"
      |> expect "Hello Foo Bar World!"

    [<Test>]
    let ``should test that function returning object is resolved`` () =
      json "{\"foo\":{\"foobar\":\"Foo Bar\"}}"
      |> dust   "Hello {#foo}{bar}{/foo} World!"
      |> expect "Hello Foo Bar World!"

module Z21_Debugger =
    
    [<Test>]
    [<ExpectedException>]
    let ``Should crash the application if a helper is not found`` () =
      empty
      |> dust   "some text {@notfound}foo{/notfound} some text"
      |> ignore

    [<Test>]
    [<ExpectedException>]
    let ``should fail hard for invalid filter`` () =
      json "{\"obj\":\"test\"}"
      |> dust  //"invalid filter"
               "{obj|nullcheck|invalid}"
      |> ignore

    [<Test>]
    [<ExpectedException>]
    let ``test the log messages for a reference not found.`` () =
      json "{\"test\":\"example text\"}"
      |> dust  //"Reference not found"
               "{wrong.test}"
      |> ignore

    // test the log messages for an unhandled section.
    [<Test>]
    [<ExpectedException>]
    let ``test the log messages for an unhandled section.`` () =
      json "{\"test\":\"example text\"}"
      |> dust  //"Section not found"
               "{#strangeSection}{/strangeSection}"
      |> ignore

    // test the log message for an exists block with no body
    [<Test>]
    [<ExpectedException>]
    let ``test the log message for an exists block with no body`` () =
      json "{\"foo\":\"foo\"}"
      |> dust  //"Exists without body"
               "{?foo/}"
      |> ignore

    // test the log message for a not-exists block with no body
    [<Test>]
    [<ExpectedException>]
    let ``test the log message for a not-exists block with no body`` () =
      empty
      |> dust  //"Not exists without body"
               "{^foo/}"
      |> ignore

    // test to make sure errors are properly caught and propogated to the callbacks.
    [<Test>]
    [<ExpectedException>]
    let ``test to make sure errors are properly caught and propogated to the callbacks.`` () =
      empty
      |> dust  //"Errors should be throwable from helpers and consumed in the render callback/stream onerror"
               "{@error errorMessage=\"helper error\"}{/error}"
      |> ignore

#endif