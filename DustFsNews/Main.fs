module Main

open News
open Suave
open Suave.Http
open Suave.Tcp
open Suave.Logging
open System.Net
open Dust.Engine

// warmup parser
parse "{!Dummy!}" |> ignore

let port = 8083us
let serverConfig =
    { Web.defaultConfig with homeFolder = Some __SOURCE_DIRECTORY__
                             logger = LiterateConsoleTarget([|"news"|], LogLevel.Info)
                             bindings = [ HttpBinding.create HTTP IPAddress.Loopback port ] }

Web.startWebServer serverConfig app
