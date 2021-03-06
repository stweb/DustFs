#if INTERACTIVE
#load "scripts/load-references-debug.fsx"
#else
module News
#endif

open System
open System.IO
open FSharp.Data
open Dust.Engine
open Dust.Suave
open Suave
open Suave.Logging
open Suave.Logging.Message
open Suave.Operators

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

type SpiegelRSS = XmlProvider<"""<?xml version="1.0" encoding="UTF-8" standalone="yes"?>
<rss xmlns:content="http://purl.org/rss/1.0/modules/content/" version="2.0">
<channel>
<title>SPIEGEL ONLINE</title>
<link>http://www.spiegel.de</link>
<description>Deutschlands führende Nachrichtenseite.</description>
<language>de</language>
<pubDate>Sun, 21 Aug 2016 14:29:00 +0200</pubDate>
<lastBuildDate>Sun, 21 Aug 2016 14:29:00 +0200</lastBuildDate>
<image>
<title>SPIEGEL ONLINE</title>
<link>http://www.spiegel.de</link>
<url>http://www.spiegel.de/static/sys/logo_120x61.gif</url>
</image>
<item>
<title>Test</title>
<link>http://www.spiegel.de/politik/</link>
<description>Lorem ipsum.</description>
<pubDate>Sun, 21 Aug 2016 12:47:00 +0200</pubDate>
<guid>http://www.spiegel.de/politik/</guid>
<content:encoded><![CDATA[]]></content:encoded>
<enclosure type="image/jpeg" url="http://www.spiegel.de/images/dummy.jpg"/>
</item>
<item>
<title>Test 2</title>
<link>http://www.spiegel.de/politik/</link>
<description>Lorem ipsum.</description>
<pubDate>Sun, 21 Aug 2016 12:47:00 +0200</pubDate>
<guid>http://www.spiegel.de/politik/</guid>
<content:encoded><![CDATA[]]></content:encoded>
</item>
</channel>
</rss>""">

let getSpiegel _ = async {
  let! res = SpiegelRSS.AsyncLoad("http://www.spiegel.de/schlagzeilen/tops/index.rss")
  return box
    [ for item in res.Channel.Items |> Seq.truncate 15 do
        match item.Enclosure with
        | Some thumb -> yield { ThumbUrl = thumb.Url; LinkUrl = item.Link;
                                Title = item.Title; Description = item.Description }
        | None -> () ] }

type RSS = XmlProvider<"""<?xml version="1.0" encoding="UTF-8"?>
<rss xmlns:dc="http://purl.org/dc/elements/1.1/" xmlns:content="http://purl.org/rss/1.0/modules/content/" xmlns:atom="http://www.w3.org/2005/Atom" version="2.0" xmlns:media="http://search.yahoo.com/mrss/">
    <channel>
        <title><![CDATA[BBC News - Home]]></title>
        <description><![CDATA[BBC News - Home]]></description>
        <link>http://www.bbc.co.uk/news/</link>
        <image>
            <url>http://news.bbcimg.co.uk/nol/shared/img/bbc_news_120x60.gif</url>
            <title>BBC News - Home</title>
            <link>http://www.bbc.co.uk/news/</link>
        </image>
        <generator>RSS for Node</generator>
        <lastBuildDate>Sat, 14 Apr 2018 11:50:44 GMT</lastBuildDate>
        <copyright><![CDATA[Copyright]]></copyright>
        <language><![CDATA[en-gb]]></language>
        <ttl>15</ttl>
        <item>
            <title><![CDATA[investigation]]></title>
            <description><![CDATA[text]]></description>
            <link>http://www.test.org</link>
            <guid isPermaLink="true">http://www.test.org</guid>
            <pubDate>Fri, 13 Apr 2018 22:33:40 GMT</pubDate>
            <media:thumbnail width="976" height="549" url="#"/>
        </item>
        <item>
            <title><![CDATA[investigation]]></title>
            <description><![CDATA[text]]></description>
            <link>http://www.test.org</link>
            <guid isPermaLink="true">http://www.test.org</guid>
            <pubDate>Fri, 13 Apr 2018 22:33:40 GMT</pubDate>
        </item>
    </channel>
</rss>""">

let getNews ctx = async {
  ctx.runtime.logger.info (eventX "Get News") 
  let! res = RSS.AsyncLoad("http://feeds.bbci.co.uk/news/rss.xml")
  ctx.runtime.logger.info (eventX "News.index - Got News")
  let news =
    [ for item in res.Channel.Items |> Seq.truncate 15 do
        let url =   match item.Thumbnail with 
                    | Some t -> t.Url
                    | None   -> "" 
        yield { ThumbUrl = url; LinkUrl = item.Link; Title = item.Title; Description = item.Description } ] 

  ctx.runtime.logger.info (eventX (sprintf "Got News %d" (List.length news)))
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


    let testHelper (c:Context) (_:BodyDict) (_:KeyValue) =
        c.Write("This is a test")

filters.["niceDate"] <- NewsHelpers.niceDate
helpers.["test"] <- NewsHelpers.testHelper

// ----------------------------------------------------------------------------
// Building asynchronous Suave server
// ----------------------------------------------------------------------------
let index getFeed : WebPart = fun ctx -> async {
    ctx.runtime.logger.info (eventX (sprintf "Getting News %s" ctx.request.url.AbsolutePath))
    // perform in parallel
    let! aNews = Async.StartChild <| getFeed ctx
    let! aTmpl = Async.StartChild <| parseToCache ctx "index.html"

    #if weather
    let! weather = _weather
    #else
    let weather = []
    #endif

    // uncomment to test waiting for async results in template
    // System.Threading.Thread.Sleep(1000)

    let! html = page aTmpl { News = aNews; Weather = weather } ctx
    return html
}

// measure time consumed by executing a web part
let timed (part : WebPart) : WebPart = fun ctx -> async {
    let sw = System.Diagnostics.Stopwatch()
    sw.Start()

    let! e = (Writers.setUserData "stopwatch" sw >=> part) ctx
    sw.Stop()
    ctx.runtime.logger.info (eventX (sprintf "timed %s %.3f [ms]" ctx.request.url.AbsolutePath (elapsedMs sw)))

    return e
}

// TODO directory is not correct when calling from interactive
templateDir <- Path.Combine(__SOURCE_DIRECTORY__ , "tmpl")
printfn "templates %s" templateDir

open Suave.Filters
open Suave.RequestErrors

let app =
  let tdir = templateDir
  choose
    [ path "/"          >=> index getNews |> timed
      path "/spiegel"   >=> index getSpiegel |> timed
      path "/style.css" >=> Writers.setMimeType "text/css" >=> Files.sendFile (tdir + "_style.css") true
      NOT_FOUND         "Found no handlers" ]
