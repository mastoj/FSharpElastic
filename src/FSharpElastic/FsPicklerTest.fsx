#I "../packages/Newtonsoft.Json.6.0.3/lib/net45"
#r "Newtonsoft.Json.dll"
#I "../packages/FsPickler.Json.0.9.6/lib/net45"
#r "FsPickler.Json.dll"
#I "../packages/FsPickler.0.9.6/lib/net45"
#r "FsPickler.dll"

open Newtonsoft.Json
open Nessos.FsPickler
open Nessos.FsPickler.Json
open System.IO

module Pickling = 
    type Y = 
        { ya : string
          yb : int }

    let res a =
        let pick = JsonPickler()
        pick.PickleToString a
    
    let y = {ya="tomas"; yb = 5}
    