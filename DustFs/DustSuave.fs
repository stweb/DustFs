﻿module Dust.Suave

open System
open System.Text
open System.IO
open Dust.Engine
open Suave
open Suave.Utils
open Suave.Http
open Suave.Logging

// global template directory
let mutable templateDir = ""

// parse template from global directory and cache
let parseToCache name = async {
    let ctx = { Context.defaults with TmplDir = templateDir }
    return ctx.ParseCached parse name
}

// render Dust page asynchronously for Suave
let page<'T> atmpl (model : 'T) ctx = async {

    let slog msg = Log.info ctx.runtime.logger "Dust" TraceHeader.empty 
                            (sprintf "%s %s" msg ctx.request.url.AbsolutePath)

    let sb = System.Text.StringBuilder()
    let dustctx = { Context.defaults with 
                        TmplDir = templateDir; 
                        W = new StringWriter(sb); 
                        Logger = slog 
                        // pass the Suave request to Dust - box'ed objects to ensure IDictionary<string,obj> compatibility
                        Global = ([("request", box ctx.request);
                                   ("host", box System.Environment.MachineName)] |> Map.ofList) 
                        Current = Some(box model);
                        }
    
    let! doc = atmpl // get parsed template async and render
    doc |> render dustctx

    let resp = Response.response HTTP_200 (Encoding.UTF8.GetBytes (sb.ToString())) ctx
    return! resp
}
