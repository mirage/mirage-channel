#!/usr/bin/env ocaml
#use "topfind"
#require "topkg"
open Topkg

let metas = [
  Pkg.meta_file ~install:false "pkg/META";
  Pkg.meta_file ~install:false "pkg/META.lwt";
]

let opams =
  let opam no_lint name =
    Pkg.opam_file ~lint_deps_excluding:(Some no_lint) ~install:false name
  in
  [
  opam ["lwt"; "mirage-channel"] "opam";
  opam ["mirage-flow"] "mirage-channel-lwt.opam";
  ]

let () =
  Pkg.describe "mirage-channel" @@ fun c ->
  match Conf.pkg_name c with
  | "mirage-channel" ->
    Ok [ Pkg.lib "pkg/META";
         Pkg.lib ~exts:Exts.interface "src/mirage_channel" ]
  | "mirage-channel-lwt" ->
    Ok [ Pkg.lib "pkg/META.lwt" ~dst:"META";
         Pkg.mllib "lwt/mirage-channel-lwt.mllib";
         Pkg.test "test/test" ]
  | other ->
    R.error_msgf "unknown package name: %s" other
