#!/usr/bin/env ocaml
#use "topfind"
#require "topkg"

open Topkg

let () =
    Pkg.describe "fusion" @@ fun c ->
        Ok [
            Pkg.mllib ~api:["Fusion"] "src/fusion.mllib";
            Pkg.test ~dir:"test" "test/fusion_test";
        ]
