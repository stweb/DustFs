module Main

open News
open Suave

let port = 8085   
let serverConfig = 
    { Web.defaultConfig with homeFolder = Some __SOURCE_DIRECTORY__
                             logger = Logging.Loggers.saneDefaultsFor Logging.LogLevel.Warn
                             bindings = [ Types.HttpBinding.mk' Types.HTTP "127.0.0.1" port ] }

Web.startWebServer serverConfig app