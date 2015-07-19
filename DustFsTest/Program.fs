module Dust.Test
 
open Dust.Engine
open System.IO
open System.Collections.Generic
open Newtonsoft.Json
open Newtonsoft.Json.Converters
open System.Dynamic

type Test = {
    name: string;
    source: string;
    context: obj;
    expected: string;
    message: string;
};

type Suite = {
    name: string;
    tests: seq<Test>;
}

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

let Suite1 =
    { name = "truth/falsy tests";
      tests = [
                {
                    name = "false value in context is treated as empty, same as undefined";
                    source = "{false}";
                    context = json """{ "false": false }""";
                    expected = "";
                    message = "should test for false in the context, evaluated and prints nothing"
                }
                {
                    name =     "numeric 0 value in context is treated as non empty";
                    source =   "{zero}";
                    context = json """{ "zero": 0 }""";
                    expected = "0";
                    message = "should test for numeric zero in the context, prints the numeric zero"
                }
                {
                    name =     "empty string context is treated as empty";
                    source =   "{emptyString}";
                    context = json """{ "emptyString": "" }""";
                    expected = "";
                    message = "should test emptyString, prints nothing"
                }
                {
                    name =     "empty string, single quoted in context is treated as empty";
                    source =   "{emptyString}";
                    context = json  """{ "emptyString": '' }""";
                    expected = "";
                    message = "should test emptyString single quoted, prints nothing"
                }
                {
                    name =     "null in the context treated as empty";
                    source =   "{NULL}";
                    context = json """{ "NULL": null }""";
                    expected = "";
                    message = "should test null in the context treated as empty"
                }
                {
                    name =     "undefined in the context treated as empty";
                    source =   "{UNDEFINED}";
                    context =  json """{"UNDEFINED": undefined }""";
                    expected =  "";
                    message = "should test undefined in the context treated as empty"
                }
                {
                    name =    "undefined string in the context treated as non empty";
                    source =   "{UNDEFINED}";
                    context =  json """ {"UNDEFINED": "undefined"}""";
                    expected = "undefined";
                    message = "should test string undefined in the context as non empty"
                }
                {
                    name =     "null is treated as empty in exists";
                    source =   "{?scalar}true{:else}false{/scalar}";
                    context = json """{"scalar": null}""";
                    expected = "false";
                    message =  "should test null as empty in exists section";
                }
                {
                    name =     "undefined is treated as empty in exists";
                    source =   "{?scalar}true{:else}false{/scalar}";
                    context =  json """{"scalar": undefined}""";
                    expected = "false";
                    message =  "should test null treated as empty in exists"
                }
                {
                    name =     "null is treated as truthy in not exists";
                    source =   "{^scalar}true{:else}false{/scalar}";
                    context =   json """{"scalar": null}""";
                    expected = "true";
                    message =  "should test null as truthy in not exists"
                }
                {
                     name =    "undefined is treated as truthy in not exists";
                     source =   "{^scalar}true{:else}false{/scalar}";
                     context = json """{"scalar": undefined}""";
                     expected = "true";
                     message =  "should test undefined as truthy in not exists"
                }
                {
                     name =    "undefined is treated as empty in exists";
                     source =   "{?scalar}true{:else}false{/scalar}";
                     context = json """{"scalar": undefined}""";
                     expected = "false";
                     message =  "should test null treated as empty in exists"
                }
            ]
    }

let Suite2 =
    { name = "core tests";
      tests = [
                {
                   name = "hello_world";
                   source = """Hello World!""";
                   context = empty;
                   expected = """Hello World!""";
                   message = "should test basic text rendering";
                }
                {
                   name = "confusing \" \n \' \u2028 \u2029 template name\\";
                   source = """Hello World!""";
                   context = empty;
                   expected = """Hello World!""";
                   message = "javascript-special characters in template names shouldn't break things";
                }
                {
                   name = "reference";
                   source = """{?one}{one}{/one}""";
                   context = json """{"one": 0 }""";
                   expected = """0""";
                   message = "should test a basic reference";
                }
                {
                   name = "implicit array";
                   source = """{#names}{.}{~n}{/names}""";
                   context = json """{ names: ["Moe", "Larry", "Curly"] }""";
                   expected = "Moe\nLarry\nCurly\n";
                   message = "should test an implicit array";
                }
                {
                   name = "TODO inline param from outer scope";
                   source = """{#person foo=root}{foo}: {name}, {age}{/person}""";
                   context = json """{ root: "Subject", person: { name: "Larry", age: 45 } }""";
                   expected = """Subject: Larry, 45""";
                   message = "should test renaming a key";
                }
                {
                   name = "force local";
                   source = """{#person}{.root}: {name}, {age}{/person}""";
                   context = json """{ root: "Subject", person: { name: "Larry", age: 45 } }""";
                   expected = """: Larry, 45""";
                   message = "should test force a key";
                }
                {
                   name = "filter un-escape";
                   source = """{safe|s}{~n}{unsafe}""";
                   context = json """{ safe: "<script>alert('Hello!')</script>", unsafe: "<script>alert('Goodbye!')</script>" }""";
                   expected = "<script>alert('Hello!')</script>\n&lt;script&gt;alert(&#39;Goodbye!&#39;)&lt;/script&gt;";
                   message = "should test escaped characters";
                }
                {
                   name = "TODO (deprecated) escape pragma";
                   source = """{%esc:s}\n  {unsafe}{~n}\n  {%esc:h}\n    {unsafe}\n  {/esc}\n{/esc}""";
                   context = json """{ unsafe: "<script>alert('Goodbye!')</script>" }""";
                   expected = """<script>alert('Goodbye!')</script>\n&lt;script&gt;alert(&#39;Goodbye!&#39;)&lt;/script&gt;""";
                   message = "should test escape_pragma";
                }
                {
                   name = "use . for creating a block and set params";
                   source = """{#. test="you"}{name} {test}{/.}""";
                   context = json """{ name: "me"}""";
                   expected = """me you""";
                   message = ". creating a block";
                }
                {
                   name = "base_template";
                   source = """Start{~n}{+title}Base Title{/title}{~n}{+main}Base Content{/main}{~n}End""";
                   context = empty;
                   expected = "Start\nBase Title\nBase Content\nEnd";
                   message = "should test base template";
                }
                {
                   name = "child_template";
                   source = """{^xhr}{>base_template/}{:else}{+main/}{/xhr}{<title}Child Title{/title}{<main}Child Content{/main}""";
                   context = json """ {xhr: false}""";
                   expected = "Start\nChild Title\nChild Content\nEnd";
                   message = "should test child template";
                }
                {
                   name = "issue322";
                   source = """hi{+"{name}"/}""";
                   context = empty;
                   expected = """hi""";
                   message = "should setup base template for next test. hi should not be part of base block name";
                }
                {
                   name = "issue322 use base template picks up prefix chunk data";
                   source = """{>issue322 name="abc"/}{<abc}ABC{/abc}""";
                   context = empty;
                   expected = """hiABC""";
                   message = "should use base template and honor name passed in";
                }
                {
                   name = "recursion";
                   source = """{name}{~n}{#kids}{>recursion:./}{/kids}""";
                   context = json """{
                            name: '1',
                            kids: [
                              {
                                  name: '1.1',
                                  kids: [
                                    {name: '1.1.1'}
                                  ]
                              }
                            ]}""";
                   expected = "1\n1.1\n1.1.1\n";
                   message = "should test recursion";
                }
                {
                   name = "comments";
                   source = """{!
                  Multiline
                  {#foo}{bar}{/foo}
                !}
                {!before!}Hello{!after!}""";
                   context = empty;
                   expected =  "Hello";
                   message = "should test comments";
                }
                {
                   name = "TODO context.resolve";
                   source = """{#foo bar="{baz} is baz " literal="literal " func=func chunkFunc="{chunkFunc}" indirectChunkFunc=indirectChunkFunc ref=ref }Fail{/foo}""";
                   context = js """{
                    foo: function(chunk, context, bodies, params) {
                        chunk.write(context.resolve(params.bar));
                        chunk.write(context.resolve(params.literal));
                        chunk.write(context.resolve(params.func));
                        chunk.write(context.resolve(params.chunkFunc));
                        chunk.write(context.resolve(params.indirectChunkFunc));
                        chunk.write(context.resolve(params.ref));
                        return chunk;
                    },
                    baz: "baz",
                    ref: "ref",
                    func: function() {
                        return "func ";
                    },
                    chunkFunc: function(chunk) {
                        return chunk.write('chunk ');
                    },
                    indirectChunkFunc: function(chunk) {
                        return chunk.write('indirect ');
                    }
                }""";
                   expected = """baz is baz literal func chunk indirect ref""";
                   message = "context.resolve() taps parameters from the context";
                }
                {
                   name = "TODO context";
                   source = """{#list:projects}{name}{:else}No Projects!{/list}""";
                   context = js """{
                    list: function(chunk, context, bodies) {
                        var items = context.current(),
                            len   = items.length;

                        if (len) {
                            chunk.write("<ul>\n");
                            for (var i=0; i<len; i++) {
                                chunk = chunk.write("<li>")
                                  .render(bodies.block, context.push(items[i]))
                                  .write("</li>\n");
                            }
                            return chunk.write("</ul>");
                        } else if (bodies['else']) {
                            return chunk.render(bodies['else'], context);
                        }
                        return chunk;
                    }""";
                   expected = """<ul>
                <li>Mayhem</li>
                <li>Flash</li>
                <li>Thunder</li>
                </ul>""";
                   message = "should test the context";
                }
            ]
    }


let Failed =
    { name = "one test";
      tests = [ 
                {
                   name = "filters";
                   source = """{title}{~n}{title|s}{~r}{~n}""";
                   context = json """{"title": '"All is <Fair> in Love & War"'}""";
                   expected = "&quot;All is &lt;Fair&gt; in Love &amp; War&quot;\n\
                               \"All is <Fair> in Love & War\"\r\n";
                   message = "should test force a key";
                }
              ]
    }


// runs one test case through the F# dust parser
let rec runTest case retry =
    let body = parse case.source
    if not (case.name.Contains " ") then // keep these in cache for inclusion
        cache.[case.name] <- body
    let data = case.context 
    let sb = System.Text.StringBuilder()
    let ctx = { _w = new StringWriter(sb); _templateDir = __SOURCE_DIRECTORY__ + """\null\"""
                data = data; index = 0; current = None; scope = [] }
    body |> List.iter(fun p -> render ctx [] p)
    let result = sb.ToString() 

    if result = case.expected then
        printfn "PASS %s" case.name
    else
        let expected = case.expected
        printfn "FAIL %s" case.name
        printfn "  Expected ´%s´\n  Actual   ´%s´" expected result
        // if retry then runTest case false

// run a test suite
let runSuite suite =
    let tests = suite.tests
    for test in tests do       
        if not (test.name.StartsWith("TODO ")) then 
            try     
                match test.context with
                | :? string as s -> let data = JsonConvert.DeserializeObject(s, new ExpandoObjectConverter())
                                    runTest {test with context = data} true
                | :? System.Dynamic.ExpandoObject as ctx -> runTest test true
                | _ -> failwith "todo"
            with
            | ex -> printfn "EXCEPTION %s: %s" test.name ex.Message

//let runUnitTests =
//    let check a b =
//        match b with
//        | Some(bb) -> if not (a.Equals(bb)) then failwith "check failed"        
//        | _ -> failwith "check failed - no value"   
//
//    let name, kv = parseKeyValue """test type
//    ="primary" name="add_product" label="action.order.now" test2=hallo """
//    //check "test" Some(name)
//    check "\"primary\"" <| kv.TryFindProp "type"
//    check "\"add_product\"" <| kv.TryFindProp "name"
//    check "hallo" <| kv.TryFindProp "test2"

// main
[<EntryPoint>]
let runTests args =
    //runSuite Failed
    runSuite Suite1 // truth/falsy tests
    runSuite Suite2 // core
    
    printfn "done"
    0
