#!/usr/bin/env ocaml
#use "topfind"
#require "topkg"
open Topkg

let () =
  Pkg.describe "mirage-channel" @@ fun c ->
  Ok [ Pkg.mllib "src/channel.mllib";
       Pkg.test "test/test"; ]
