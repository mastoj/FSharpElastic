namespace FSharpElastic.FsPicklerTest

open Newtonsoft.Json
open Nessos.FsPickler
open Nessos.FsPickler.Json
open System.IO

module Pickling = 
    let res y = 
        let jsp = FsPickler.CreateJson()
        use textWriter = new StringWriter()
        jsp.Serialize(textWriter, y)
        textWriter.ToString()
