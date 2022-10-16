open Yojson
open Yojson.Basic.Util

type scale = { key : string; tonality : int list }
type piano = { piano : char list }
type notes = { notes : char list }
type seed = int list

let rec generate_seed_helper lst length range : seed =
  if length = 0 then lst
  else
    let n = Random.int range in
    let lst1 = lst @ [ n ] in
    generate_seed_helper lst1 (length - 1) range

let generate_seed (length : int) (range : int) : seed =
  generate_seed_helper [] length range
