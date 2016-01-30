module Todo

open Dust.Engine
open Dust.Test
open NUnit.Framework
open NUnit.Framework.Constraints
open FsUnit
open System.IO

#if TODO

module T02_CoreTests =
    // should render the template name
    [<Test>]
    [<Ignore "requires JS helper">]
    let ``should render the template name`` () =
      json "{}"
      |> dust  "global_template"
               "{#helper foo=\"bar\" boo=\"boo\"} {/helper}"
      |> expect "global_template"

    // should render the template name with paths
    [<Test>]
    [<Ignore "requires JS helper">]   
    let ``should render the template name with paths`` () =
      json "{}"
      |> dust  "apps/test/foo.tl&v=0.1"
               "{#helper foo=\"bar\" boo=\"boo\" template=\"tl/apps/test\"} {/helper}"
      |> expect "apps/test/foo.tl&v=0.1"

    // should test renaming a key
    [<Test>]
    [<Ignore "implement renaming">]
    let ``should test renaming a key`` () =
      json "{\"root\":\"Subject\",\"person\":{\"name\":\"Larry\",\"age\":45}}"
      |> dust  "inline param from outer scope"
               "{#person foo=root}{foo}: {name}, {age}{/person}"
      |> expect "Subject: Larry, 45"

    // should test escape_pragma
    [<Test>]
    [<Ignore "implement{%esc}">]
    let ``should test escape_pragma`` () =
      json "{\"unsafe\":\"<script>alert(\'Goodbye!\')</script>\"}"
      |> dust  "escape pragma"
               "{%esc:s}\n  {unsafe}{~n}\n  {%esc:h}\n    {unsafe}\n  {/esc}\n{/esc}"
      |> expect "<script>alert(\'Goodbye!\')</script>\n&lt;script&gt;alert(&#39;Goodbye!&#39;)&lt;/script&gt;"

    // . creating a block
    [<Test>]
    [<Ignore("TODO implement ad-hoc blocks")>]
    let ``dot creating a block`` () =
      json "{\"name\":\"me\"}"
      |> dust  "use . for creating a block and set params"
               "{#. test=\"you\"}{name} {test}{/.}"
      |> expect "me you"

    // should functions in context
    [<Test>]
    [<Ignore "requires JS in context">]
    let ``should functions in context`` () =
      json "{}"
      |> dust  "functions in context"
               "Hello {type} World!"
      |> expect "Hello Sync World!"

    // should test functions in context
    [<Test>]
    [<Ignore "requires JS in context">]
    let ``should test functions in context`` () =
      json "{}"
      |> dust  "async functions in context"
               "Hello {type} World!"
      |> expect "Hello Async World!"

    // should test sync chunk write
    [<Test>]
    [<Ignore "requires JS in context">]
    let ``should test sync chunk write`` () =
      json "{}"
      |> dust  "sync chunk write test"
               "Hello {type} World!"
      |> expect "Hello Chunky World!"

    // should test recursion
    [<Test>]    
    [<Ignore "Requires JS Context">]
    let ``should test recursion`` () =
      json "{\"name\":\"1\",\"kids\":[{\"name\":\"1.1\",\"kids\":[{\"name\":\"1.1.1\"}]}]}"
      |> dustReg  "recursion" 
               "{name}{~n}{#kids}{>recursion:./}{/kids}"        
      |> expect "1\n1.1\n1.1.1\n"

    // context.resolve() taps parameters from the context
    [<Test>]
    [<Ignore "Requires JS Context">]
    let ``context_resolve() taps parameters from the context`` () =
      json "{\"baz\":\"baz\",\"ref\":\"ref\"}"
      |> dust  "context.resolve"
               "{#foo bar=\"{baz} is baz \" literal=\"literal \" func=func chunkFunc=\"{chunkFunc}\" indirectChunkFunc=indirectChunkFunc ref=ref }Fail{/foo}"
      |> expect "baz is baz literal func chunk indirect ref"

    // should test the context
    [<Test>]
    [<Ignore "Requires JS Context">]
    let ``should test the context`` () =
      json "{\"projects\":[{\"name\":\"Mayhem\"},{\"name\":\"Flash\"},{\"name\":\"Thunder\"}]}"
      |> dust  "context"
               "{#list:projects}{name}{:else}No Projects!{/list}"
      |> expect "<ul>\n<li>Mayhem</li>\n<li>Flash</li>\n<li>Thunder</li>\n</ul>"

    // should allow pushing and popping a context    
    [<Test>]
    [<Ignore "Requires JS Context">]
    let ``should allow pushing and popping a context`` () =
      json "{}"
      |> dust  "context push / pop"
               "{#helper}{greeting} {firstName} {lastName}{.}{/helper}"
      |> expect "Hello Dusty Dusterson!"

    // should allow cloning a context
    [<Test>]
    [<Ignore "Requires JS Context">]
    let ``should allow cloning a context`` () =
      json "{}"
      |> dust  "context clone"
               "{#helper}{greeting} {firstName} {lastName}{/helper}"
      |> expect "Hello Dusty Dusterson"



[<Ignore "Implement thenable/promises">]
module T07_ObjectTestsWithThenable =

    // should reserve an async chunk for a thenable reference
    [<Test>]
    let ``should reserve an async chunk for a thenable reference`` () =
      json "{\"magic\":{}}"
      |> dust  "thenable reference"
               "Eventually {magic}!"
      |> expect "Eventually magic!"

    // undefined
    [<Test>]
    let ``undefined`` () =
      json "{\"rice-krispies\":{}}"
      |> dust  "thenable escape reference"
               "{rice-krispies} {rice-krispies|s}"
      |> expect "Snap, Crackle &amp; Pop Snap, Crackle & Pop"

    // should deep-inspect a thenable reference
    [<Test>]
    let ``should deep-inspect a thenable reference`` () =
      json "{\"magic\":{}}"
      |> dust  "thenable deep reference"
               "Eventually {magic.ally.delicious}!"
      |> expect "Eventually Lucky Charms!"

    // should deep-inspect a thenable reference but move on if it isn't there
    [<Test>]
    let ``should deep-inspect a thenable reference but move on if it isn't there`` () =
      json "{\"magic\":{}}"
      |> dust  "thenable deep reference that doesn\'t exist"
               "Eventually {magic.ally.disappeared}!"
      |> expect "Eventually !"

    // should deep-inspect a thenable reference recursively
    [<Test>]
    let ``should deep-inspect a thenable reference recursively`` () =
      json "{\"magic\":{}}"
      |> dust  "thenable deep reference... this is just getting silly"
               "Eventually {magic.ally.delicious}!"
      |> expect "Eventually Lucky Charms!"

    // should inspect a thenable reference but move on if it fails
    [<Test>]
    let ``should inspect a thenable reference but move on if it fails`` () =
      json "{\"magic\":{}}"
      |> dust  "thenable reference that fails"
               "Eventually {magic.ally.delicious}!"
      |> expect "Eventually !"

    // should deep-inspect a thenable reference but move on if it fails
    [<Test>]
    let ``should deep-inspect a thenable reference but move on if it fails`` () =
      json "{\"magic\":{}}"
      |> dust  "thenable deep reference that fails"
               "Eventually {magic.ally.delicious}!"
      |> expect "Eventually !"

    // should reserve an async section for a thenable
    [<Test>]
    let ``should reserve an async section for a thenable`` () =
      json "{\"promise\":{}}"
      |> dust  "thenable section"
               "{#promise}Eventually {magic}!{/promise}"
      |> expect "Eventually magic!"

    // should iterate over an array resolved from a thenable
    [<Test>]
    let ``should iterate over an array resolved from a thenable`` () =
      json "{\"promise\":{}}"
      |> dust  "thenable resolves with array into reference"
               "{promise}"
      |> expect "foo,bar,baz"

    // should iterate over an array resolved from a thenable
    [<Test>]
    let ``should iterate over an array resolved from a thenable 2`` () =
      json "{\"promise\":{}}"
      |> dust  "thenable resolves with array into section"
               "{#promise}{name}{/promise}"
      |> expect "foobarbaz"

    // Should not render a thenable section with no body
    [<Test>]
    let ``Should not render a thenable section with no body`` () =
      json "{\"promise\":{}}"
      |> dust  "thenable empty section"
               "{#promise/}"
      |> expect ""

    // should reserve an async section for a thenable returned from a function
    [<Test>]
    let ``should reserve an async section for a thenable returned from a function`` () =
      json "{}"
      |> dust  "thenable section from function"
               "{#promise}Eventually {magic}!{/promise}"
      |> expect "Eventually magic!"

    // should reserve an async section for a deep-reference thenable
    [<Test>]
    let ``should reserve an async section for a deep-reference thenable`` () =
      json "{\"magic\":{}}"
      |> dust  "thenable deep section"
               "Eventually my {#magic.ally}{delicious}{/magic.ally} will come"
      |> expect "Eventually my Lucky Charms will come"

    // should reserve an async section for a deep-reference thenable and not blow the stack
    [<Test>]
    let ``should reserve an async section for a deep-reference thenable and not blow the stack`` () =
      json "{\"prince\":\"Prince\",\"magic\":{}}"
      |> dust  "thenable deep section, traverse outside"
               "Eventually my {#magic.ally}{prince} {delicious} {charms}{/magic.ally} will come"
      |> expect "Eventually my Prince Lucky Charms will come"

    // Dust helpers that return thenables are resolved in context
    [<Test>]
    let ``Dust helpers that return thenables are resolved in context`` () =
      empty
      |> dust  "thenable resolved by global helper"
               "{@promise resolve=\"helper\"}I am a big {.}!{/promise}"
      |> expect "I am a big helper!"

    // Dust helpers that return thenables are rejected in context
    [<Test>]
    let ``Dust helpers that return thenables are rejected in context`` () =
      empty
      |> dust  "thenable rejected by global helper"
               "{@promise reject=\"error\"}I am a big helper!{:error}I am a big {.}!{/promise}"
      |> expect "I am a big error!"

    // rejected thenable reference logs
    [<Test>]
    let ``rejected thenable reference logs`` () =
      json "{\"promise\":{}}"
      |> dust  "thenable error"
               "{promise}"
      |> expect "undefined"

    // rejected thenable renders error block
    [<Test>]
    let ``rejected thenable renders error block`` () =
      json "{\"promise\":{}}"
      |> dust  "thenable error with error block"
               "{#promise}No magic{:error}{message}{/promise}"
      |> expect "promise error"

[<Ignore "Implement streams">]
module T07_ObjectTestsWithStreams =

    // should reserve an async chunk for a stream reference
    [<Test>]
    let ``should reserve an async chunk for a stream reference`` () =
      json "{}"
      |> dust  "stream"
               "Stream of {stream}..."
      |> expect "Stream of consciousness..."

    // should respect filters set on stream references
    [<Test>]
    let ``should respect filters set on stream references`` () =
      json "{}"
      |> dust  "stream escaping"
               "{polluted|s} {polluted}"
      |> expect "<&> &lt;&amp;&gt;"

    // should abort the stream if it raises an error
    [<Test>]
    let ``should abort the stream if it raises an error`` () =
      json "{}"
      |> dust  "stream error"
               "{stream}..."
      |> expect "Everything is..."

    // should reserve an async section for a stream
    [<Test>]
    let ``should reserve an async section for a stream`` () =
      json "{}"
      |> dust  "stream section"
               "Pour {#molecule}{atom}{num}{/molecule} in the glass"
      |> expect "Pour H2O in the glass"

    // should reserve an async chunk for a stream reference and abort if the stream errors
    [<Test>]
    let ``should reserve an async chunk for a stream reference and abort if the stream errors`` () =
      json "{}"
      |> dust  "stream section error"
               "Pour {#molecule}{atom}{num}{:error}{message}{/molecule} in the glass"
      |> expect "Pour H2O... no! in the glass"

    // should render streams found while iterating over an array
    [<Test>]
    let ``should render streams found while iterating over an array`` () =
      json "{\"streams\":[null,null,null]}"
      |> dust  "array of streams"
               "{#streams}{.} {/streams}"
      |> expect "Danube Rhine Seine "

    // should seamlessly mix asynchronous data sources
    [<Test>]
    let ``should seamlessly mix asynchronous data sources`` () =
      json "{\"water\":{}}"
      |> dust  "promise a stream and stream a promise"
               "Little Bobby drank and drank, and then he drank some more. But what he thought was {water} was {sulfuric_acid}!"
      |> expect "Little Bobby drank and drank, and then he drank some more. But what he thought was H2O was H2SO4!"

    // should not treat MongoDB documents as streams
    [<Test>]
    let ``should not treat MongoDB documents as streams`` () =
      json "{\"mongo\":{\"name\":\"Mongo\"}}"
      |> dust  "MongoDB-like Document is not a stream"
               "{#mongo}{name}{/mongo}"
      |> expect "Mongo"

    // stream section with no body should not render
    [<Test>]
    let ``stream section with no body should not render`` () =
      json "{}"
      |> dust  "Stream section with no body should not render"
               "{#stream/}"
      |> expect ""

module T07_NestedPaths =
    // === SUITE ===nested path tests
    // should test the leading dot behavior in local mode
    [<Test>]
    let ``should test the leading dot behavior in local mode`` () =
      json "{\"name\":\"List of people\",\"age\":\"8 hours\",\"people\":[{\"name\":\"Alice\"},{\"name\":\"Bob\",\"age\":42}]}"
      |> dust  "Verify local mode leading dot path in local mode"
               "{#people}{.name} is {?.age}{.age} years old.{:else}not telling us their age.{/age}{/people}"
      |> expect "Alice is not telling us their age.Bob is 42 years old."

    // should test explicit context blocks (=prevents) looking further up stack
    [<Test>] // TODO set cur = true inside explicit # -> how?
    let ``should test explicit context blocks looking further up stack`` () =
      json "{\"data\":{\"A\":{\"name\":\"Al\",\"list\":[{\"name\":\"Joe\"},{\"name\":\"Mary\"}],\"B\":{\"name\":\"Bob\",\"Blist\":[\"BB1\",\"BB2\"]}},\"C\":{\"name\":\"cname\"}}}"
      |> dust  "same as previous test but with explicit context"
               "{#data.A:B}Aname{name}{data.C.name}{/data.A}"
      |> expect "AnameAl"

    // should test access global despite explicit context
    [<Test>]
    [<Ignore "TODO Implement global">]
    let ``should test access global despite explicit context`` () =
      json "{\"data\":{\"A\":{\"name\":\"Al\",\"list\":[{\"name\":\"Joe\"},{\"name\":\"Mary\"}],\"B\":{\"name\":\"Bob\",\"Blist\":[\"BB1\",\"BB2\"]}},\"C\":{\"name\":\"cname\"}}}"
      |> dust  "explicit context but gets value from global"
               "{#data.A:B}Aname{name}{glob.globChild}{/data.A}"
      |> expect "AnameAltestGlobal"

    // Should find glob.globChild which is in context.global
    [<Test>]
    [<Ignore "TODO Implement global">]
    let ``Should find glob_globChild which is in context_global`` () =
      empty
      // base: { glob: { globChild: "testGlobal"} },        
      |> dust  "check nested ref in global works in global mode"
               "{glob.globChild}"
      |> expect "testGlobal"

    // Should find glob.globChild which is in context.global
    [<Test>]
    [<Ignore "TODO Implement global">]
    let ``Should find glob_globChild which is in context_global 2`` () =
      json "{\"data\":{\"A\":{\"name\":\"Al\",\"B\":\"Ben\",\"C\":{\"namex\":\"Charlie\"}},\"C\":{\"namey\":\"Charlie Sr.\"}}}"
      |> dust  "check nested ref not found in global if partial match"
               "{#data}{#A}{C.name}{/A}{/data}"
      |> expect ""


    // should test resolve correct 'this' when invoking method
    [<Ignore "Requires JavaScript">]
    [<Test>]
    let ``should test resolve correct 'this' when invoking method`` () =
      json "{\"person\":{\"firstName\":\"Peter\",\"lastName\":\"Jones\", \"fullName\": function() {
                return this.firstName + ' ' + this.lastName;
            }}}"
      |> dust  "method invocation"
               "Hello {person.fullName}"
      |> expect "Hello Peter Jones"

    // Should resolve path correctly
    [<Test>]
    [<Ignore "implement array index references">]
    let ``Should resolve index path correctly`` () =
      json "{\"nulls\":[1,null,null,2],\"names\":[{\"name\":\"Moe\"},{\"name\":\"Curly\"}]}"
      |> dust  "check null values in section iteration do not break path resolution"
               "{#nulls}{names[0].name}{/nulls}"
      |> expect "MoeMoeMoeMoe"

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
      |> expect "FOO BAR"

    // should escapeJs a string when using the j filter
    [<Test>]
    let ``should escapeJs a string when using the j filter`` () =
      json "{\"obj\":\"<script>\\\\testBS\\\\ \\rtestCR\\r \u2028testLS\u2028 \u2029testPS\u2029 \\ntestNL\\n \\ftestLF\\f \'testSQ\' \\ttestTB\\t /testFS/</script>\"}"
      |> dust  "escapeJs filter without DQ"
               "{obj|j|s}"
      |> expect "<script>\\\\testBS\\\\ \\rtestCR\\r \\u2028testLS\\u2028 \\u2029testPS\\u2029 \\ntestNL\\n \\ftestLF\\f \\\'testSQ\\\' \\ttestTB\\t \\/testFS\\/<\\/script>"

    // should escapeJs a string with double quotes when using the j filter
    [<Test>]
    let ``should escapeJs a string with double quotes when using the j filter`` () =
      json "{\"obj\":\"\\\"testDQ\\\"\"}"
      |> dust  "escapeJs filter with only DQ"
               "{obj|j|s}"
      |> expect "\\\"testDQ\\\""

    // should stringify a JSON literal when using the js filter
    [<Test>]
    let ``should stringify a JSON literal when using the js filter`` () =
      json "{\"obj\":{\"id\":1,\"name\":\"bob\",\"occupation\":\"construction\"}}"
      |> dust  "escapeJSON filter"
               "{obj|js|s}"
      |> expect "{\"id\":1,\"name\":\"bob\",\"occupation\":\"construction\"}"

    // should escape bad characters when using the js filter
    [<Test>]
    let ``should escape bad characters when using the js filter`` () =
      json "{\"obj\":{\"name\":\"<<\u2028testLS \u2029testPS\"}}"
      |> dust  "escapeJSON filter with bad characters"
               "{obj|js|s}"
      |> expect "{\"name\":\"\\u003c\\u003c\\u2028testLS \\u2029testPS\"}"

    // should objectify a JSON string when using the jp filter
    [<Test>]
    let ``should objectify a JSON string when using the jp filter`` () =
      json "{\"obj\":\"{\\\"id\\\":1,\\\"name\\\":\\\"bob\\\",\\\"occupation\\\":\\\"construction\\\"}\"}"
      |> dust  "JSON.parse filter"
               "{obj|jp}"
      |> expect "[object Object]"

    // filters are passed the current context
    [<Test>]
    let ``filters are passed the current context`` () =
      json "{\"woo\":0,\"name\":\"Boo\",\"dust\":{\"woo\":5,\"name\":\"Dust\"}}"
      |> dust  "filter receives context"
               "{#dust}{name|woo}{/dust}"
      |> expect "DUST!!!!!"

module T11_PartialParams =

    // === SUITE ===partial/params tests
    // should test partials
    [<Test>]
    let ``should test partials`` () =
      json "{\"name\":\"Jim\",\"count\":42,\"ref\":\"hello_world\"}"
      |> dust  "partials"
               "{>partial foo=0 /} {>\"hello_world\" foo=1 /} {>\"{ref}\" foo=2 /}"
      |> expect "Hello Jim! You have 42 new messages. Hello World! Hello World!"

    // should test partial with an asynchronously-resolved template name
    [<Test>]
    let ``should test partial with an asynchronously-resolved template name`` () =
      json "{}"
      |> dust  "partial with async ref as name"
               "{>\"{ref}\" /}"
      |> expect "Hello World!"

    // should test partial with context
    [<Test>]
    let ``should test partial with context`` () =
      json "{\"profile\":{\"name\":\"Mick\",\"count\":30}}"
      |> dust  "partial with context"
               "{>partial:.profile/}"
      |> expect "Hello Mick! You have 30 new messages."

    // partial with blocks, with no default values for blocks
    [<Test>]
    let ``partial with blocks, with no default values for blocks`` () =
      json "{\"name\":\"Mick\",\"count\":30}"
      |> dust  "partial with blocks, with no default values for blocks"
               "{>partial_with_blocks_and_no_defaults/}"
      |> expect "Hello Mick! You have 30 new messages."

    // partial with blocks, with no default values for blocks, but override default values with inline partials
    [<Test>]
    let ``partial with blocks, with no default values for blocks, but override default values with inline partials`` () =
      json "{\"name\":\"Mick\",\"count\":30}"
      |> dust  "partial with blocks, with no default values for blocks, but override default values with inline partials"
               "{>partial_with_blocks_and_no_defaults/}{<header}override header {/header}"
      |> expect "override header Hello Mick! You have 30 new messages."

    // partial with blocks, override default values with inline partials
    [<Test>]
    let ``partial with blocks, override default values with inline partials`` () =
      json "{\"name\":\"Mick\",\"count\":30}"
      |> dust  "partial with blocks, override default values with inline partials"
               "{>partial_with_blocks/}{<header}my header {/header}"
      |> expect "my header Hello Mick! You have 30 new messages."

    // should test partial with inline params
    [<Test>]
    let ``should test partial with inline params`` () =
      json "{\"n\":\"Mick\",\"c\":30}"
      |> dust  "partial with inline params"
               "{>partial name=n count=\"{c}\"/}"
      |> expect "Hello Mick! You have 30 new messages."

    // should test partial with inline params tree walk up
    [<Test>]
    let ``should test partial with inline params tree walk up`` () =
      json "{\"n\":\"Mick\",\"x\":30,\"a\":{\"b\":{\"c\":{\"d\":{\"e\":\"1\"}}}}}"
      |> dust  "partial with inline params tree walk up"
               "{#a}{#b}{#c}{#d}{>partial name=n count=\"{x}\"/}{/d}{/c}{/b}{/a}"
      |> expect "Hello Mick! You have 30 new messages."

    // should test partial with inline params and context
    [<Test>]
    let ``should test partial with inline params and context`` () =
      json "{\"profile\":{\"n\":\"Mick\",\"c\":30}}"
      |> dust  "partial with inline params and context"
               "{>partial:profile name=\"{n}\" count=\"{c}\"/}"
      |> expect "Hello Mick! You have 30 new messages."

    // should test partial with inline params and context tree walk up
    [<Test>]
    let ``should test partial with inline params and context tree walk up`` () =
      json "{\"profile\":{\"n\":\"Mick\",\"x\":30,\"a\":{\"b\":{\"c\":{\"d\":{\"e\":\"1\"}}}}}}"
      |> dust  "partial with inline params and context tree walk up"
               "{#profile}{#a}{#b}{#c}{#d}{>partial:profile name=n count=\"{x}\"/}{/d}{/c}{/b}{/a}{/profile}"
      |> expect "Hello Mick! You have 30 new messages."

    // should test partial with literal inline param and context. Fallback values for name or count are undefined
    [<Test>]
    let ``should test partial with literal inline param and context; Fallback values for name or count are undefined`` () =
      json "{\"profile\":{\"n\":\"Mick\",\"count\":30}}"
      |> dust  "partial with literal inline param and context"
               "{>partial:profile name=\"Joe\" count=\"99\"/}"
      |> expect "Hello Joe! You have 30 new messages."

    // should test partial with dynamic name and a context
    [<Test>]
    let ``should test partial with dynamic name and a context`` () =
      json "{\"partialName\":\"partial\",\"me\":{\"name\":\"Mick\",\"count\":30}}"
      |> dust  "partial with dynamic name and context"
               "{>\"{partialName}\":me /}"
      |> expect "Hello Mick! You have 30 new messages."

    // should test partial with dynamic name and a context
    [<Test>]
    let ``should test partial with dynamic name and a context 2`` () =
      json "{\"partialName\":\"partial\",\"me\":{\"name\":\"Mick\",\"count\":30}}"
      |> dust  "partial with dynamic name and context and inline params"
               "{>\"{partialName}\" name=me.name count=me.count /}"
      |> expect "Hello Mick! You have 30 new messages."

    // should preserve partials backwards compatibility with compilers pre-2.7
//    [<Test>]
//    [<Ignore("doesn't apply to dustfs")>]
//    let ``should preserve partials backwards compatibility with compilers pre 2_7`` () =
//      json "{\"name\":\"Mick\",\"count\":30}"
//      |> dust  "backcompat (< 2.7.0) compiler with no partial context"
//               "{#oldPartial/}"
//      |> expect "Hello Mick! You have 30 new messages."

    // should test partial with blocks and inline params
    [<Test>]
    let ``should test partial with blocks and inline params`` () =
      json "{\"n\":\"Mick\",\"c\":30}"
      |> dust  "partial with blocks and inline params"
               "{>partial_with_blocks name=n count=\"{c}\"/}"
      |> expect "default header Hello Mick! You have 30 new messages."

    // should test partial with blocks, override default values for blocks and inline params
    [<Test>]
    let ``should test partial with blocks, override default values for blocks and inline params`` () =
      json "{\"n\":\"Mick\",\"c\":30}"
      |> dust  "partial with blocks, override default values for blocks and inline params"
               "{>partial_with_blocks name=n count=\"{c}\"/}{<header}my header {/header}"
      |> expect "my header Hello Mick! You have 30 new messages."

    // should test partial blocks and no defaults, override default values for blocks and inline params
    [<Test>]
    let ``should test partial blocks and no defaults, override default values for blocks and inline params`` () =
      json "{\"n\":\"Mick\",\"c\":30}"
      |> dust  "partial with blocks and no defaults, override default values for blocks and inline params"
               "{>partial_with_blocks_and_no_defaults name=n count=\"{c}\"/}{<header}my header {/header}"
      |> expect "my header Hello Mick! You have 30 new messages."

    // should test partial with no blocks, ignore the override inline partials
    [<Test>]
    let ``should test partial with no blocks, ignore the override inline partials`` () =
      json "{\"n\":\"Mick\",\"c\":30}"
      |> dust  "partial with no blocks, ignore the override inline partials"
               "{>partial name=n count=\"{c}\"/}{<header}my header {/header}"
      |> expect "Hello Mick! You have 30 new messages."

    // should print the current template name
    [<Test>]
    let ``should print the current template name`` () =
      json "{}"
      |> dust  "partial prints the current template name"
               "{>partial_print_name/}"
      |> expect "partial_print_name"

    // should print the current dynamic template name
    [<Test>]
    let ``should print the current dynamic template name`` () =
      json "{\"partial_print_name\":\"partial prints the current template name\"}"
      |> dust  "partial prints the current dynamic template name"
               "{>\"{partial_print_name}\"/}"
      |> expect "partial_print_name"

    // should print the current template name
    [<Test>]
    let ``should print the current template name 2`` () =
      json "{}"
      |> dust  "nested partial prints the current template name"
               "{>nested_partial_print_name/}"
      |> expect "partial_print_name"

    // should print the current template name with some additional output
    [<Test>]
    let ``should print the current template name with some additional output`` () =
      json "{\"parentTemplate\":\"parent\",\"parentSource\":\"{?undefinedVar}{:else}{>\\\"content\\\"/}{/undefinedVar}\",\"contentTemplate\":\"content\",\"contentSource\":\"templateName: {#printTemplateName}{/printTemplateName} output: additional output\"}"
      |> dust  "nested partial 2 levels deep from loadSource prints the current template name"
               "{#loadTemplate name=\"{contentTemplate}\" source=\"{contentSource|s}\"}{/loadTemplate}\n{#loadTemplate name=\"{parentTemplate}\" source=\"{parentSource|s}\"}{/loadTemplate}\n{>\"{parentTemplate}\"/} | additional parent output"
      |> expect "templateName: content output: additional output | additional parent output"

    // should render the helper with missing global context
    [<Test>]
    let ``should render the helper with missing global context`` () =
      json "{}"
      |> dust  "partial with makeBase_missing_global"
               "{#helper template=\"partial\"}{/helper}"
      |> expect "Hello ! You have  new messages."

    // Should gracefully handle stepping into context that does not exist
    [<Test>]
    let ``Should gracefully handle stepping into context that does not exist`` () =
      json "{}"
      |> dust  "partial stepping into context that does not exist"
               "{#loadPartialTl}{/loadPartialTl}\n{>partialTl:contextDoesNotExist/}"
      |> expect " "

module T12_InlineParams =

    // === SUITE ===inline params tests
    // should test inner params
    [<Test>]
    [<Ignore("requires JS & Block handlers")>]
    let ``should test inner params`` () =
      json "{}"
      // context:  {  helper: function(chunk, context, bodies, params) { return chunk.write(params.foo); } },
      |> dust  "params"
               "{#helper foo=\"bar\"/}"
      |> expect "bar"

    // Block handlers syntax should support integer number parameters
    [<Test>]
    [<Ignore("requires JS & Block handlers")>]
    let ``Block handlers syntax should support integer number parameters`` () =
      json "{}"
      // context:  { helper: function(chunk, context, bodies, params) { return chunk.write(params.foo); } },
      |> dust  "inline params as integer"
               "{#helper foo=10 /}"
      |> expect "10"

    // Block handlers syntax should support decimal number parameters
    [<Test>]
    [<Ignore("requires JS & Block handlers")>]
    let ``Block handlers syntax should support decimal number parameters`` () =
      json "{}"
      // context:  { helper: function(chunk, context, bodies, params) { return chunk.write(params.foo); } },       
      |> dust  "inline params as float"
               "{#helper foo=3.14159 /}"
      |> expect "3.14159"

    // should print negative integer
    [<Test>]
    let ``should print negative integer`` () =
      json "{\"foo\":true}"
      |> dust  "inline params as negative integer"
               "{#foo bar=-1}{bar}{/foo}"
      |> expect "-1"

    // should print negative float
    [<Test>]
    let ``should print negative float`` () =
      json "{\"foo\":true}"
      |> dust  "inline params as negative float"
               "{#foo bar=-1.1}{bar}{/foo}"
      |> expect "-1.1"

    // should test parameters with dashes
    [<Test>]
    [<Ignore("requires JS & Block handlers")>]
    let ``should test parameters with dashes`` () =
      json "{}"
      // context:  { helper: function(chunk, context, bodies, params) { return chunk.write(params['data-foo']); } },
      |> dust  "inline params with dashes"
               "{#helper data-foo=\"dashes\" /}"
      |> expect "dashes"

    // Inline params that evaluate to a dust function should evaluate their body
    [<Test>]
    let ``Inline params that evaluate to a dust function should evaluate their body`` () =
      json "{\"section\":true,\"b\":\"world\"}"
      |> dust  "inline params as dust function"
               "{#section a=\"{b}\"}{#a}Hello, {.}!{/a}{/section}"
      |> expect "Hello, world!"

module T13_InlinePartialBlock =

    // === SUITE ===inline partial/block tests
    // should test blocks with dynamic keys
    [<Test>]
    let ``should test blocks with dynamic keys`` () =
      json "{\"val\":\"A\"}"
      |> dust  "blocks with dynamic keys"
               "{<title_A}\nAAA\n{/title_A}\n{<title_B}\nBBB\n{/title_B}\n{+\"title_{val}\"/}"
      |> expect "AAA"

    // should test blocks with more than one dynamic keys
    [<Test>]
    let ``should test blocks with more than one dynamic keys`` () =
      json "{\"val1\":\"title\",\"val2\":\"A\"}"
      |> dust  "blocks with more than one dynamic keys"
               "{<title_A}\nAAA\n{/title_A}\n{<title_B}\nBBB\n{/title_B}\n{+\"{val1}_{val2}\"/}"
      |> expect "AAA"

    // should test blocks with dynamic key values as objects
    [<Test>]
    let ``should test blocks with dynamic key values as objects`` () =
      json "{\"val1\":\"title\",\"val2\":\"A\",\"obj\":{\"name\":\"B\"}}"
      |> dust  "blocks with dynamic key values as objects"
               "{<title_A}\nAAA\n{/title_A}\n{<title_B}\nBBB\n{/title_B}\n{+\"{val1}_{obj.name}\"/}"
      |> expect "BBB"

    // should test blocks with dynamic key values as arrays
    [<Test>]
    let ``should test blocks with dynamic key values as arrays`` () =
      json "{\"val1\":\"title\",\"val2\":\"A\",\"obj\":{\"name\":[\"A\",\"B\"]}}"
      |> dust  "blocks with dynamic key values as arrays"
               "{<title_A}\nAAA\n{/title_A}\n{<title_B}\nBBB\n{/title_B}\n{+\"{val1}_{obj.name[0]}\"/}"
      |> expect "AAA"

[<Ignore("TODO requires JavaScript in context")>]
module T14_Lambda =

    // === SUITE ===lambda tests
    // should test that a non-chunk return value is used for truthiness
    [<Test>]
    let ``should test that a non-chunk return value is used for truthiness`` () =
      json "{\"foo\":{\"foobar\":\"Foo Bar\"}}"
      |> dust  "test that the scope of the function is correct and that a non-chunk return value is used for truthiness checks"
               "Hello {#foo}{#bar}{.}{/bar}{/foo} World!"
      |> expect "Hello Foo Bar World!"

    // should functions that return false are falsy
    [<Test>]
    let ``should functions that return false are falsy`` () =
      json "{}"
      |> dust  "test that function that do not return chunk and return falsy are treated as falsy"
               "{#bar}{.}{:else}false{/bar}"
      |> expect "false"

    // should functions that return 0 are truthy
    [<Test>]
    let ``should functions that return 0 are truthy`` () =
      json "{}"
      |> dust  "test that function that do not return chunk and return 0 are treated as truthy (in the Dust sense)"
               "{#bar}{.}{:else}false{/bar}"
      |> expect "0"

    // should test scope of context function
    [<Test>]
    let ``should test scope of context function`` () =
      json "{\"foo\":{\"foobar\":\"Foo Bar\"}}"
      |> dust  "test that the scope of the function is correct"
               "Hello {#foo}{bar}{/foo} World!"
      |> expect "Hello Foo Bar World!"

    // should test that function returning object is resolved
    [<Test>]
    let ``should test that function returning object is resolved`` () =
      json "{\"foo\":{\"foobar\":\"Foo Bar\"}}"
      |> dust  "test that function returning object is resolved"
               "Hello {#foo}{bar}{/foo} World!"
      |> expect "Hello Foo Bar World!"

module T15_CoreGrammar =
    // === SUITE ===core-grammar tests
    // should ignore extra whitespaces between opening brace plus any of (#,?,@,^,+,%) and the tag identifier
    [<Test>]
    let ``should ignore extra whitespaces between opening brace plus any of (#,?,at,^,+,%) and the tag identifier`` () =

      helpers.["helper"] <- (fun (c:Context) (bodies:BodyDict) (param:KeyValue) (renderBody: unit -> unit) ->
                                match param.TryFind "boo" with
                                | Some v -> c.Write (v.ToString()) // TODO
                                | None -> failwith "missing key"
                            )
      json "{}"
      |> dust  "ignore extra whitespaces between opening brace plus any of (#,?,@,^,+,%) and the tag identifier"
               "{# helper foo=\"bar\" boo=\"boo\" } {/helper}"
      |> expect "boo bar"

    // should show an error for whitespces between the opening brace and any of (#,?,@,^,+,%)
    [<Test>]
    let ``should show an error for whitespaces between the opening brace and any of (#,?,at,^,+,%)`` () =
      json "{}"
      |> dust  "error: whitespaces between the opening brace and any of (#,?,@,^,+,%) is not allowed"
               "{ # helper foo=\"bar\" boo=\"boo\" } {/helper}"
      |> expect "undefined"

    // should ignore extra whitespaces between the closing brace plus slash and the tag identifier
    [<Test>]
    let ``should ignore extra whitespaces between the closing brace plus slash and the tag identifier`` () =
      json "{}"
      |> dust  "whitespaces between the closing brace plus slash and the tag identifier is supported"
               "{# helper foo=\"bar\" boo=\"boo\"} {/ helper }"
      |> expect "boo bar"

    // should show an error because whitespaces between the '{' and the forward slash are not allowed in the closing tags
    [<Test>]
    let ``should show an error because whitespaces between the '{' and the forward slash are not allowed in the closing tags`` () =
      json "{}"
      |> dust  "error: whitespaces between the openning curly brace and forward slash in the closing tags not supported"
               "{# helper foo=\"bar\" boo=\"boo\"} { / helper }"
      |> expect "undefined"

    // should ignore extra whitespaces before the self closing tags
    [<Test>]
    let ``should ignore extra whitespaces before the self closing tags`` () =
      json "{}"
      |> dust  "whitespaces before the self closing tags is allowed"
               "{#helper foo=\"bar\" boo=\"boo\" /}"
      |> expect "boo bar"

    // should show an error for whitespaces  etween the forward slash and the closing brace in self closing tags
    [<Test>]
    let ``should show an error for whitespaces  etween the forward slash and the closing brace in self closing tags`` () =
      json "{}"
      |> dust  "error: whitespaces between the forward slash and the closing brace in self closing tags"
               "{#helper foo=\"bar\" boo=\"boo\" / }"
      |> expect "undefined"

    // should ignore extra whitespaces between inline params
    [<Test>]
    let ``should ignore extra whitespaces between inline params`` () =
      json "{}"
      |> dust  "extra whitespaces between inline params supported"
               "{#helper foo=\"bar\"   boo=\"boo\"/}"
      |> expect "boo bar"

    // should show an error for whitespaces between the '{' plus '>' and partial identifier
    [<Test>]
    let ``should show an error for whitespaces between the '{' plus '>' and partial identifier`` () =
      json "{\"name\":\"Jim\",\"count\":42,\"ref\":\"hello_world\"}"
      |> dust  "error : whitespaces between the \'{\' plus \'>\' and partial identifier is not supported"
               "{ > partial/} {> \"hello_world\"/} {> \"{ref}\"/}"
      |> expect "undefined"

    // should ignore extra whitespacesbefore the forward slash and the closing brace in partials
    [<Test>]
    let ``should ignore extra whitespacesbefore the forward slash and the closing brace in partials`` () =
      json "{\"name\":\"Jim\",\"count\":42,\"ref\":\"hello_world\"}"
      |> dust  "whitespaces before the forward slash and the closing brace in partials supported"
               "{>partial /} {>\"hello_world\" /} {>\"{ref}\" /}"
      |> expect "Hello Jim! You have 42 new messages. Hello World! Hello World!"

    // should test dash in partial's keys
    [<Test>]
    [<Ignore("requires Inline parsing and reolution")>]
    let ``should test dash in partial's keys`` () =
      json "{\"foo-title\":\"title\",\"bar-letter\":\"a\"}"
      |> dust  "support dash in partial\'s key"
               "{<title-a}foo-bar{/title-a}{+\"{foo-title}-{bar-letter}\"/}"
      |> expect "foo-bar"


[<Ignore("TODO")>]
module T16_SyntaxError =
    // === SUITE ===syntax error tests
    // should test that the error message shows line and column.
    [<Test>]
    let ``should test that the error message shows line and column`` () =
      json "{\"name\":\"Mick\",\"count\":30}"
      |> dust  "Dust syntax error"
               "RRR {##}"
      |> expect "undefined"

    // should test the errors message for section with error.
    [<Test>]
    let ``should test the errors message for section with error`` () =
      empty
      |> dust  "Dust syntax error. Error in Section"
               "{#s}\n{#&2}\n{/s}"
      |> expect "undefined"

    // should test the errors message for section with a buffer and error inside.
    [<Test>]
    let ``should test the errors message for section with a buffer and error inside`` () =
      empty
      |> dust  "Dust syntax error. Error in Section with buffer"
               "{#s}\nthis is the\nbuffer\n{#&2}\na second\nbuffer\n{/s}"
      |> expect "undefined"

    // should test the errors message for section without end tag shows.
    [<Test>]
    let ``should test the errors message for section without end tag shows`` () =
      empty
      |> dust  "Dust syntax error. Error in Section without end tag"
               "{#s}\nthis is the\nbuffer\na second\nbuffer"
      |> expect "undefined"

    // should test the errors message for partials with a buffer inside.
    [<Test>]
    let ``should test the errors message for partials with a buffer inside`` () =
      empty
      |> dust  "Dust syntax error. Error in Partial with buffer"
               "{+header}\nthis is a Partial\nwith Error\neeee{@#@$fdf}\ndefault header \n{/header}"
      |> expect "undefined"

    // should test the errors message for partial without end tag.
    [<Test>]
    let ``should test the errors message for partial without end tag`` () =
      empty
      |> dust  "Dust syntax error. Error in Partial without end tag"
               "{+header}\nthis is the\nbuffer\na second\nbuffer"
      |> expect "undefined"

    // should test the errors message for Scalar.
    [<Test>]
    let ``should test the errors message for Scalar`` () =
      empty
      |> dust  "Dust syntax error. Error in Scalar"
               "{#scalar}\ntrue\n {#@#fger}\n{:else}\nfalse\n{/scalar}"
      |> expect "undefined"

    // should test the errors message for Scalar.
    [<Test>]
    let ``should test the errors message for Scalar 2`` () =
      empty
      |> dust  "Dust syntax error. Error in Scalar\'s else"
               "{#scalar}\ntrue\n{:else}\nfalse\n {#@#fger}\n{/scalar}"
      |> expect "undefined"

    // should test the errors message for Conditionals.
    [<Test>]
    let ``should test the errors message for Conditionals`` () =
      empty
      |> dust  "Dust syntax error. Error in Conditional"
               "{?tags}\n<ul>{~n}\n{#tags}{~s}\n<li>{#@$}</li>{~n}\n{/tags}\n</ul>\n{:else}\nNo Tags!\n{/tags}"
      |> expect "undefined"

    // should test the errors message for Conditional's else.
    [<Test>]
    let ``should test the errors message for Conditionals else`` () =
      empty
      |> dust  "Dust syntax error. Error in Conditional\'s else"
               "{?tags}\n<ul>{~n}\n{#tags}{~s}\n<li>{.}</li>{~n}\n{/tags}\n</ul>\n{:else}\n{#@$}\nNo Tags!\n{/tags}"
      |> expect "undefined"

    // should test the errors message for Conditional without end tag.
    [<Test>]
    let ``should test the errors message for Conditional without end tag`` () =
      empty
      |> dust  "Dust syntax error. Error in Conditional without end tag"
               "{?tags}\n<ul>{~n}\n{#tags}{~s}\n<li>{.}</li>{~n}\n{/tags}\n</ul>\n{:else}\nNo Tags!"
      |> expect "undefined"

    // should test helper syntax errors being handled gracefully
    [<Test>]
    let ``should test helper syntax errors being handled gracefully`` () =
      json "{}"
      |> dust  "Helper syntax error. TypeError"
               "{#hello/}"
      |> expect "undefined"

    // should test helper syntax errors inside an async block being handled gracefully
    [<Test>]
    let ``should test helper syntax errors inside an async block being handled gracefully`` () =
      json "{}"
      |> dust  "Helper syntax error. async TypeError"
               "{#hello/}"
      |> expect "undefined"



module R18_WhitespaceOn =
    // === SUITE ===whitespace test
    // whitespace on: whitespace-only template is preserved
    [<Test>]
    let ``whitespace on: whitespace-only template is preserved`` () =
      empty
      |> dust  "whitespace on: whitespace-only template"
               "\n     "
      |> expect "\n     "

    // whitespace on: whitespace-only block is preserved
    [<Test>]
    let ``whitespace on: whitespace-only block is preserved`` () =
      empty
      |> dust  "whitespace on: whitespace-only block"
               "{<foo}\n{/foo}{+foo/}"
      |> expect "\n"    
      
    // whitespace on: multiline text block should maintain indent
    [<Test>]
    let ``whitespace on: multiline text block should maintain indent`` () =
      empty
      |> dust  "whitespace on: multiline text block"
               "<p>\n    foo bar baz\n    foo bar baz\n</p>"
      |> expect "<p>\n    foo bar baz\n    foo bar baz\n</p>"

    // whitespace on: partials should preserve indentation
    [<Test>]
    let ``whitespace on: partials should preserve indentation`` () =
      empty
      |> dust  "whitespace on: partial indentation"
               "<html>\n<head>\n</head>\n<body>{+body/}<body>\n</html>\n{<body}\n    <h1>Title</h1>\n    <p>Content...</p>\n{/body}"
      |> expect "<html>\n<head>\n</head>\n<body>\n    <h1>Title</h1>\n    <p>Content...</p>\n<body>\n</html>\n"

module T20_Helper =
    // === SUITE ===helper tests
    // helper can return a primitive
    [<Test>]
    let ``helper can return a primitive`` () =
      empty
      |> dust  "helper returns a primitive"
               "{@val value=3/}"
      |> expect "3"

    // helper can return a primitive and render a body
    [<Test>]
    let ``helper can return a primitive and render a body`` () =
      empty
      |> dust  "helper returns a primitive and renders a body"
               "{@val value=\"world\"}Hello {.}{/val}"
      |> expect "Hello world"

    // helper that returns an array iterates its body
    [<Test>]
    let ``helper that returns an array iterates its body`` () =
      json "{\"arr\":[{\"name\":\"Alice\"},{\"name\":\"Bob\"},{\"name\":\"Charlie\"}]}"
      |> dust  "helper returns an array and iterates a body"
               "{@val value=arr}Hello {name} {/val}"
      |> expect "Hello Alice Hello Bob Hello Charlie "

    // helper escapes returned primitives
    [<Test>]
    let ``helper escapes returned primitives`` () =
      empty
      |> dust  "helper escapes a primitive"
               "{@val value=\"You & I\"/}"
      |> expect "You &amp; I"

    // helper applies filters to returned primitives
    [<Test>]
    let ``helper applies filters to returned primitives`` () =
      empty
      |> dust  "helper filters a primitive"
               "{@val value=\"You & I\" filters=\"|s\"/} {@val value=\"& Tim\" filters=\"|js|s\"/}"
      |> expect "You & I \"& Tim\""

    // helper filters a primitive using an array of filters
    [<Test>]
    let ``helper filters a primitive using an array of filters`` () =
      json "{\"filters\":[\"js\",\"s\"]}"
      |> dust  "helper filters a primitive using an array of filters"
               "{@val value=\"You & I\" filters=filters/}"
      |> expect "\"You & I\""

    // helper can return a Chunk
    [<Test>]
    let ``helper can return a Chunk`` () =
      json "{\"hello\":\"<Hello>\"}"
      |> dust  "helper returns a chunk"
               "{@val value=\"{hello} & world\"/}"
      |> expect "&lt;Hello&gt; & world"

    // helper doesn't apply filters to a Chunk
    [<Test>]
    let ``helper doesn't apply filters to a Chunk`` () =
      json "{\"hello\":\"<Hello>\"}"
      |> dust  "helper doesn\'t filter a chunk"
               "{@val value=\"{hello} & world\" filters=\"|s\"/}"
      |> expect "&lt;Hello&gt; & world"

    // helper applies filter from esc pragma
    [<Test>]
    let ``helper applies filter from esc pragma`` () =
      empty
      |> dust  "helper filters are affected by pragma"
               "{%esc:s}{@val value=\"You & I\"/}{/esc}"
      |> expect "You & I"

    // helper filters supercede filter from esc pragma
    [<Test>]
    let ``helper filters supercede filter from esc pragma`` () =
      empty
      |> dust  "helper filters supercede pragma"
               "{%esc:s}{@val value=\"You & I\" filters=\"|h\" /}{/esc}"
      |> expect "You &amp; I"

    // templates compiled with Dust < 2.7.1 escape values returned from helpers
    [<Test>]
    let ``templates compiled with Dust < 2.7.1 escape values returned from helpers`` () =
      json "{}"
      |> dust  "Dust < 2.7.1 compat: helpers escape references"
               "{#returnLegacy value=\"You & I\" /}"
      |> expect "You &amp; I"

[<Ignore("TODO")>]
module T21_Debugger =
    // === SUITE ===debugger tests
    // Should crash the application if a helper is not found
    [<Test>]
    let ``Should crash the application if a helper is not found`` () =
      empty
      |> dust  "non-existing helper"
               "some text {@notfound}foo{/notfound} some text"
      |> expect "undefined"

    // should fail hard for invalid filter
    [<Test>]
    let ``should fail hard for invalid filter`` () =
      json "{\"obj\":\"test\"}"
      |> dust  "invalid filter"
               "{obj|nullcheck|invalid}"
      |> expect "undefined"

    // test the log messages for a reference not found.
    [<Test>]
    let ``test the log messages for a reference not found.`` () =
      json "{\"test\":\"example text\"}"
      |> dust  "Reference not found"
               "{wrong.test}"
      |> expect "undefined"

    // test the log messages for an unhandled section.
    [<Test>]
    let ``test the log messages for an unhandled section.`` () =
      json "{\"test\":\"example text\"}"
      |> dust  "Section not found"
               "{#strangeSection}{/strangeSection}"
      |> expect "undefined"

    // test the log message for an exists block with no body
    [<Test>]
    let ``test the log message for an exists block with no body`` () =
      json "{\"foo\":\"foo\"}"
      |> dust  "Exists without body"
               "{?foo/}"
      |> expect "undefined"

    // test the log message for a not-exists block with no body
    [<Test>]
    let ``test the log message for a not-exists block with no body`` () =
      empty
      |> dust  "Not exists without body"
               "{^foo/}"
      |> expect "undefined"

    // test to make sure errors are properly caught and propogated to the callbacks.
    [<Test>]
    let ``test to make sure errors are properly caught and propogated to the callbacks.`` () =
      empty
      |> dust  "Errors should be throwable from helpers and consumed in the render callback/stream onerror"
               "{@error errorMessage=\"helper error\"}{/error}"
      |> expect "undefined"

#endif