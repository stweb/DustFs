# Dust.fs - F# templating inspired by Dust.js

Dust.fs implements a [Dust.js](http://www.dustjs.com/) compatible templating engine using [FSharp](http://fsharp.org) which works great with [Suave.io](http://suave.io) and standalone.

I've used Dust in Node.js projects and believe it to be a pretty good syntax and concept for templating which I would like to use in .net based projects, too. The existing C# based port [X-dust](https://github.com/dannichols/x-dust) seemed quite complex, yet less complete.

This is my first bigger F# project and it was inspired by Tomas Petricek's chapter in the excellent book [F# Deep Dives](http://functional-programming.net/deepdives/) and uses Active Pattern Matching.

When I saw his talk [End-to-end functional web development (June) - NDC Oslo 2015](http://tpetricek.github.io/Talks/2015/end-to-end-web/ndc/#/) featuring a demo using the C# based dotliquid engined, I decided to port his [news example](https://github.com/tpetricek/Talks/tree/master/2015/end-to-end-web/ndc/code-done/news) over to Dust.fs and publish it.

## Technical Summary

Dust.fs parses textual Dust templates into an AST (Abstract Syntax Tree) representation using F# Discriminated Unions. The template is rendered by traversing the AST. Templates can be cached in memory and partial templates are handled separately and combined during rendering. The data can be provided as .NET object tree, supporting dynamic (ExpandoObjects) as well as F# Maps and IDictionary.

Even without further optimization, I've seen up to 10x faster rendering compared to dotliquid. Depending on your hardware the news page renders in <1ms (excluding data collection).

Helper functions and Filters can be implemented in F#, similar to the original.

The current implementation is considered a working Proof-of-Concept but needs more work, especially unit test coverage to ensure dust.js compatibility.

## Known Bugs & Limitations

- Not all standard filters are implemented
- Helpers use a slightly different signature and the API needs to be elaborated.
- The parser may not be 100% accepting the same input, although I used the orginal [PEGjs syntax](https://github.com/linkedin/dustjs/blob/master/src/dust.pegjs) as a reference and tested the code on a number of complex examples.
- Evaluation of references needs to be modified to pass all orginal scenarios after unit tests have been added. (The orginal tests are based on Javascript and I tried connecting those in via Edge.js) 

## Build & Run

> dotnet build

> dotnet run --project DustFsNews/DustFsNews.fsproj

## Contributing

Feedback and Contributions are welcome.
