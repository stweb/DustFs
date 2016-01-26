open News
open Suave
open Suave.Http
open Suave.Tcp
open System.Net

let port = 8083us
let serverConfig =
    { Web.defaultConfig with homeFolder = Some __SOURCE_DIRECTORY__
                             logger = Logging.Loggers.saneDefaultsFor Logging.LogLevel.Info
                             bindings = [ HttpBinding.mk HTTP IPAddress.Loopback port ] }

Web.startWebServer serverConfig app
