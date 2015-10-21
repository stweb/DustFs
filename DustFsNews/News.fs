module News

open System
open System.IO
open FSharp.Data
open Dust.Engine
open Dust.Suave
open Suave
open Suave.Utils
open Suave.Types
open Suave.Http
open Suave.Http.Successful
open Suave.Http.RequestErrors
open Suave.Http.Applicatives

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
  { News : seq<News>
    Weather : seq<Weather> }

// ----------------------------------------------------------------------------
// Getting Weather information and formatting it
// ----------------------------------------------------------------------------

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

// ----------------------------------------------------------------------------
// Getting News from RSS feed and formatting it
// ----------------------------------------------------------------------------

type SpiegelRSS = XmlProvider<"http://www.spiegel.de/schlagzeilen/tops/index.rss">

let getSpiegel = async {
  let! res = SpiegelRSS.AsyncGetSample()
  return
    [ for item in res.Channel.Items |> Seq.take 15 do
        match item.Enclosure with
        | Some thumb -> yield { ThumbUrl = thumb.Url; LinkUrl = item.Link;
                                Title = item.Title; Description = item.Description }
        | None -> () ] }

type RSS = XmlProvider<"http://feeds.bbci.co.uk/news/rss.xml">

let getNews = async {
  let! res = RSS.AsyncGetSample()
  return
    [ for item in res.Channel.Items |> Seq.take 15 do
      if item.Thumbnails |> Seq.length > 0 then
        let thumb = item.Thumbnails |> Seq.maxBy (fun t -> t.Width)
        yield
          { ThumbUrl = thumb.Url; LinkUrl = item.Link;
            Title = item.Title; Description = item.Description } ] }

// ----------------------------------------------------------------------------
// Dust templating
// ----------------------------------------------------------------------------

templateDir <- __SOURCE_DIRECTORY__ + """/tmpl/""" 

module NewsHelpers =
    let niceDate (v:obj) : obj = 
        match v with
        | :? DateTime as dt -> dt.ToLongDateString() :> obj
                                //dt.ToUniversalTime().ToString("yyyy'-'MM'-'dd'T'HH':'mm':'ss'.'fff'Z'") :> obj
        | _ -> failwith "bad type"


    let versionHelper (c:Context) (bodies:BodyDict) (param:KeyValue) (renderBody: unit -> unit) =    
        c.Write("This is a test")

filters.["niceDate"] <- NewsHelpers.niceDate
helpers.["version"] <- NewsHelpers.versionHelper

// ----------------------------------------------------------------------------
// Building asynchronous Suave server
// ----------------------------------------------------------------------------
let index getFeed : WebPart = fun ctx -> async { 
    let sw = System.Diagnostics.Stopwatch()
    sw.Start()
    // perform in parallel
    let! _news = Async.StartChild getFeed
    let! _weather = Async.StartChild getWeather
    let! _tmpl = Async.StartChild (parseToCache "index.html")
    // await results
    let! news = _news
    let! weather = _weather
    let e1 = elapsedMs sw

    let! html = page _tmpl { News = news; Weather = weather } ctx
    sw.Stop()
    System.Console.WriteLine("{0} data {1:N3} render {2:N3} [ms]", ctx.request.url, e1, elapsedMs sw - e1)
    return html
}

let app =
  choose
    [ path "/" >>= Writers.setMimeType "text/html" >>= index getNews
      path "/spiegel" >>= Writers.setMimeType "text/html" >>= index getSpiegel
      path "/style.css" >>= Writers.setMimeType "text/css" >>= file (templateDir + "_style.css")
      NOT_FOUND "Found no handlers" ]
