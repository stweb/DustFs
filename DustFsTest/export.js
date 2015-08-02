// Node.js script to export Dust.js test cases to F# for use in DustFs 
// placed in root of dustjs code repository

var fs = require("fs"),
	tests = require("./test/templates/all.js")

// http://stackoverflow.com/questions/4994201/is-object-empty    
// Speed up calls to hasOwnProperty
var hasOwnProperty = Object.prototype.hasOwnProperty;

function isEmpty(obj) {

    // null and undefined are "empty"
    if (obj == null) return true;

    // Assume if it has a length property with a non-zero value
    // that that property is correct.
    if (obj.length > 0)    return false;
    if (obj.length === 0)  return true;

    // Otherwise, does it have any properties of its own?
    // Note that this doesn't handle
    // toString and valueOf enumeration bugs in IE < 9
    for (var key in obj) {
        if (hasOwnProperty.call(obj, key)) return false;
    }

    return true;
}

// check if variable is a function    
function isFunction(functionToCheck) {
 var getType = {};
 return functionToCheck && getType.toString.call(functionToCheck) === '[object Function]';
}

// escape strings for F# code literals, quoted
// http://www.tutorialspoint.com/fsharp/fsharp_strings.htm
function fsEsc(str) {
  return '"' + ('' + str).replace(/["'\\\n\r\u2028\u2029]/g, function (character) {
    // Escape all characters not included in SingleStringCharacters and
    // DoubleStringCharacters on
    // http://www.ecma-international.org/ecma-262/5.1/#sec-7.8.4
    switch (character) {
      case '"':
      case "'":
      case '\\':
        return '\\' + character
      // Four possible LineTerminator characters need to be escaped:
      case '\n':
        return '\\n' // TODO consider using \n\ and multiple lines for readability
      case '\r':
        return '\\r'
      case '\u2028':
        return '\\u2028'
      case '\u2029':
        return '\\u2029'
    }
  }) + '"'
}

// create the output stream
var out = fs.createWriteStream("tests.fs");
out.on('error', function(err) {
  console.log("ERROR:" + err);
});

// export suites to F# tests
var suite, i, j, test;

for(i = 0; i < tests.length; i++) {
    suite = tests[i];
    
    // new suite - TODO create a module name automatically
    console.log("SUITE " + suite.name);
    out.write("\n");
    out.write("// === SUITE ===" + suite.name);
    out.write("\n");
    
    for(j = 0; j < suite.tests.length; j++) {
        test = suite.tests[j];
        console.log(test.name);
    
        // for now we skip tests with JS code in context, need to be translated manually to F# or use Edgejs    
        // NOTE: some dustjs tests depend on cached templates from previous case
        if (!isFunction(test.context))
        {
            // output assumes FSUnit / NUnit testing and helper functions for readability
            out.write("// " + test.message);
            out.write("\n[<Test>]\n");
            
            // TODO ensure unique names
            out.write("let ``" + test.message + "`` () =\n");
            if (isEmpty(test.context)) {
                out.write('  empty\n');                
            } else {
                out.write('  json ' + fsEsc(JSON.stringify(test.context)) + '\n');                
            }
            out.write('  |> dust  ' + fsEsc(test.name) + '\n');
            out.write('           ' + fsEsc(test.source) + '\n');
            out.write('  |> should equal ' + fsEsc(test.expected) + '\n');
            out.write("\n");
        }
        else
            out.write("// SKIPPED " + test.name + "\n");
    }
}

// ensure files gets flushed to disk, do not call .close()
out.end();

console.log("Done.");