module Dust.Suave

open System
open System.IO
open Dust.Engine
open Suave
open Suave.Utils
open Suave.Types
open Suave.Http
open Suave.Http.Successful
open Suave.Http.RequestErrors
open Suave.Http.Applicatives
open Suave.Logging

let file name =
    if File.Exists name then
        OK (File.ReadAllText(name))
    else
        printf "not found %s" name
        NOT_FOUND name

let fileData name =
    if File.Exists name then
        ok (File.ReadAllBytes(name))
    else
        printf "not found %s" name
        NOT_FOUND name

let mutable templateDir = ""

let parseToCache name = async {
    let ctx = { _templateDir = templateDir; _w = null; data = null; index = 0; current = None; scope = [] }
    try
      return ctx.parseCached parse name
    with
      e -> return [] // TODO error handling
}

let page<'T> atmpl (model : 'T) r = async {

    let sb = System.Text.StringBuilder()
    let ctx = { _templateDir = templateDir; _w = new StringWriter(sb); data = (box model); index = 0; current = None; scope = [] }
    let! doc = atmpl
    doc |> List.iter(fun p -> render ctx [] p)

    let content = sb.ToString()
#if DEBUG2
    File.WriteAllText(__SOURCE_DIRECTORY__ + "/out.html", content)
#endif
    return! Response.response HTTP_200 (UTF8.bytes content) r
}
