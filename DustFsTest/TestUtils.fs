﻿module TestUtils

open Dust.Engine
open NUnit.Framework
open System

module U01_Conversion =       
    [<Test>]
    let ``can convert Value`` () =      
      let data = [ ("key", VInline "inline") ] |> Map.ofSeq
      
      let str : string = data?key
      Assert.AreEqual("inline", str)

      let v : Value = data?key
      Assert.AreEqual(VInline "inline", v)

      // NOT possible 
      //let id : Identifier = data?key
      //Assert.AreEqual(Key("inline"), id)

    [<Test>]
    let ``can convert VIdent to Key`` () =      
      let data = [ ("key", VIdent(Key "ident")) ] |> Map.ofSeq
      
      let str : string = data?key
      Assert.AreEqual("ident", str)

      let v : Value = data?key
      Assert.AreEqual(VIdent(Key "ident"), v)

      let id : Identifier = data?key
      Assert.AreEqual(Key("ident"), id)
      

//module U20_Misc =
//
//    [<Test>]
//    let ``helper interpol`` () =
//      json """{ "user": { "name": "Test User", "email": "nobody@test.org" } }"""
//      |> dust """{@html_a href=user.email href_html="mailto:{user.email|s}"/}"""
//      |> expect """<a href="nobody@test.org">"""
