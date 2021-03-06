﻿module Dust.Suave

open System.Collections.Concurrent
open System.Text
open System.IO
open Dust.Engine
open Suave
open Suave.Logging
open Suave.Logging.Message

// global template directory
let mutable templateDir = ""

let helpers = new ConcurrentDictionary<string, Helper>()

// parse template from global directory and cache
let parseToCache ctx name = async {
    let slog msg = ctx.runtime.logger.info (eventX (sprintf "%s %s" msg ctx.request.url.AbsolutePath))

    let dustctx = { Context.defaults with TmplDir = templateDir; Logger = slog }
    return dustctx.ParseCached parse name
}

// render Dust page asynchronously for Suave
let page<'T> (atmpl:Async<Body option>) (model : 'T) ctx = async {

    let slog msg = ctx.runtime.logger.info (eventX
                        (sprintf "%s %s" msg ctx.request.url.AbsolutePath))

    let sb = System.Text.StringBuilder()
    let dustctx = { Context.defaults with
                        TmplDir = templateDir;
                        W = new StringWriter(sb);
                        Logger = slog
                        // pass the Suave request to Dust - box'ed objects to ensure IDictionary<string,obj> compatibility
                        Global = ([("request", box ctx.request);
                                   ("ctx", box ctx);
                                   ("host", box System.Environment.MachineName)] |> Map.ofList)
                        Current = Some(box model);
                        Helpers = helpers
                  }
    let! doc = atmpl // get parsed template async and render
    return! match doc with
            | None      ->  RequestErrors.NOT_FOUND "template not found" ctx
            | Some body ->  try
                                body |> dustctx.Render
                                Response.response HTTP_200 (Encoding.UTF8.GetBytes (sb.ToString())) ctx
                            with
                            | e -> 
                                ctx.runtime.logger.error (eventX e.Message)
                                ServerErrors.INTERNAL_ERROR "something went wrong" ctx
}
