open Yojson
open Yojson.Basic.Util

type scale = { key : string; tonality : int list }
type piano = { piano : char list }
type notes = { notes : char list }
