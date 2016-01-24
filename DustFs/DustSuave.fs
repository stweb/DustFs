module Dust.Suave

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
    try
      return ctx.ParseCached parse name
    with
      e -> return [] // TODO error handling
}

// render Dust page asynchronously for Suave
let page<'T> atmpl (model : 'T) ctx = async {

    let slog msg = Log.info ctx.runtime.logger "Dust" TraceHeader.empty 
                            (sprintf "%s %s" msg ctx.request.url.AbsolutePath)

    let sb = System.Text.StringBuilder()
    let dustctx = { Context.defaults with 
                        TmplDir = templateDir; 
                        W = new StringWriter(sb); 
                        Data = (box model); 
                        Logger = slog }
    
    let! doc = atmpl // get parsed template async and render
    doc |> List.iter(fun p -> render dustctx [] p)

    let resp = Response.response HTTP_200 (Encoding.UTF8.GetBytes (sb.ToString())) ctx
    return! resp
}
