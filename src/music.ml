open Yojson
open Yojson.Basic.Util

type scale = { key : string; tonality : int list }
type piano = string list
type notes = string list
type seed = int list

exception UnknownKey of string

let rec generate_seed_helper lst length range : seed =
  if length = 0 then lst
  else
    let n = Random.int range in
    let lst1 = lst @ [ n ] in
    generate_seed_helper lst1 (length - 1) range

let generate_seed (length : int) (range : int) : seed =
  generate_seed_helper [] length range

let rec acc_key_to_int (acc : int) (key : string) = function
  | [] -> raise (UnknownKey key)
  | h :: t -> if String.equal h key then acc else acc_key_to_int (acc + 1) key t

let key_to_int (key : string) (piano : piano) = acc_key_to_int 0 key piano

let rec tonality_to_indexes (curr_index : int) = function
  | [] -> []
  | h :: t ->
      let new_index = (curr_index + h) mod 12 in
      new_index :: tonality_to_indexes new_index t

let sorted_note_indexes (piano : piano) (scale : scale) : int list =
  scale.tonality
  |> tonality_to_indexes (key_to_int scale.key piano)
  |> List.sort_uniq compare

let rec acc_create_notes (curr_index : int) (piano : piano)
    (sorted_note_indexes : int list) : notes =
  match (piano, sorted_note_indexes) with
  | [], _ -> []
  | _, [] -> []
  | hp :: tp, hi :: ti ->
      if hi = curr_index then hp :: acc_create_notes (curr_index + 1) tp ti
      else acc_create_notes (curr_index + 1) tp (hi :: ti)

let create_notes (piano : piano) (scale : scale) : notes =
  sorted_note_indexes piano scale |> acc_create_notes 0 piano
