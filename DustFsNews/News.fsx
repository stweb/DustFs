#if INTERACTIVE
#load "scripts/load-references-debug.fsx"
#endif

open System
open System.IO
open FSharp.Data
open Dust.Engine
open Dust.Suave
open Suave
open Suave.Logging
open Suave.Http

// ----------------------------------------------------------------------------
// Domain model for the F# Times homepage
// ----------------------------------------------------------------------------

type News =
  { ThumbUrl : string
    LinkUrl : string
    Title : string
    Description : string }

type Weather =
  { Date : DateTime
    Icon : string
    Day : int
    Night : int }

type Home =
  { News :    Async<obj> // this is an F# promise
    Weather : seq<Weather> }

// ----------------------------------------------------------------------------
// Getting Weather information and formatting it - requires API key now
// ----------------------------------------------------------------------------
#if weather
type Forecast = JsonProvider<"http://api.openweathermap.org/data/2.5/forecast/daily?q=London,UK&mode=json&units=metric&cnt=10">

let toDateTime (timestamp:int) =
  let start = DateTime(1970,1,1,0,0,0,DateTimeKind.Utc)
  start.AddSeconds(float timestamp).ToLocalTime()

let getWeather = async {
  try
      let! res = Forecast.AsyncLoad("http://api.openweathermap.org/data/2.5/forecast/daily?q=Cologne,Germany&mode=json&units=metric&cnt=10")
      return
        [ for item in res.List ->
          { Date = toDateTime item.Dt
            Icon = item.Weather.[0].Icon
            Day = int item.Temp.Day
            Night = int item.Temp.Night } ]
  with e -> return []
}
#endif

// ----------------------------------------------------------------------------
// Getting News from RSS feed and formatting it
// ----------------------------------------------------------------------------

type SpiegelRSS = XmlProvider<"http://www.spiegel.de/schlagzeilen/tops/index.rss">

let getSpiegel ctx = async {
  let! res = SpiegelRSS.AsyncGetSample()
  return box
    [ for item in res.Channel.Items |> Seq.take 15 do
        yield { ThumbUrl = item.Enclosure.Url; LinkUrl = item.Link;
                Title = item.Title; Description = item.Description } 
    ] }       

type RSS = XmlProvider<"http://feeds.bbci.co.uk/news/rss.xml">

let getNews ctx = async {
  Log.info ctx.runtime.logger "News.index" TraceHeader.empty "Get News"
  let! res = RSS.AsyncGetSample()
  Log.info ctx.runtime.logger "News.index" TraceHeader.empty "Got News"
  let news =
    [ for item in res.Channel.Items |> Seq.take 15 do
        yield
          { ThumbUrl = item.Thumbnail.Url; LinkUrl = item.Link;
            Title = item.Title; Description = item.Description } ] 

  Log.info ctx.runtime.logger "News.index" TraceHeader.empty (sprintf "Got News %d" (List.length news))
  return box news  
}

// ----------------------------------------------------------------------------
// Dust templating
// ----------------------------------------------------------------------------

module NewsHelpers =
    let niceDate (v:obj) : obj =
        match v with
        | :? DateTime as dt -> dt.ToLongDateString() :> obj
                                //dt.ToUniversalTime().ToString("yyyy'-'MM'-'dd'T'HH':'mm':'ss'.'fff'Z'") :> obj
        | _ -> failwith "bad type"


    let testHelper (c:Context) (bodies:BodyDict) (param:KeyValue) (renderBody: unit -> unit) =
        c.Write("This is a test")

filters.["niceDate"] <- NewsHelpers.niceDate
helpers.["test"] <- NewsHelpers.testHelper

// ----------------------------------------------------------------------------
// Building asynchronous Suave server
// ----------------------------------------------------------------------------
let index getFeed : WebPart = fun ctx -> async {
    Log.info ctx.runtime.logger "News.index" TraceHeader.empty (sprintf "Getting News %s" ctx.request.url.AbsolutePath)
    // perform in parallel
    let! aNews = Async.StartChild <| getFeed ctx
    let! aTmpl = Async.StartChild <| parseToCache ctx "index.html"

    #if weather
    let! weather = _weather
    #else
    let weather = []
    #endif

    System.Threading.Thread.Sleep(1000)

    let! html = page aTmpl { News = aNews; Weather = weather } ctx
    return html
}

// measure time consumed by executing a web part
let timed (part : WebPart) : WebPart = fun ctx -> async {
    let sw = System.Diagnostics.Stopwatch()
    sw.Start()

    let! e = part ctx

    sw.Stop()
    Log.verbose ctx.runtime.logger "timed" TraceHeader.empty 
       (sprintf "timed %s %.3f [ms]" ctx.request.url.AbsolutePath (elapsedMs sw))

    return e
}

// TODO directory is not correct when calling from interactive
templateDir <- Path.Combine(__SOURCE_DIRECTORY__ , "tmpl")
printfn "templates %s" templateDir

open Suave.Operators
open Suave.Filters
open Suave.RequestErrors

let app =
  let tdir = templateDir
  choose
    [ path "/"          >=> index getNews |> timed
      path "/spiegel"   >=> index getSpiegel |> timed
      path "/style.css" >=> Writers.setMimeType "text/css" >=> Files.sendFile (tdir + "_style.css") true
      NOT_FOUND         "Found no handlers" ]
