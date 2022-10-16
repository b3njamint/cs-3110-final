open Yojson
open Yojson.Basic.Util

exception UnknownKey of string

type scale = { key : string; tonality : int list }
type piano = string list
type notes = string list
type seed = int list

let rec acc_key_to_int (acc : int) (key : string) = function
  | [] -> raise (UnknownKey key)
  | h :: t -> if String.equal h key then acc else acc_key_to_int (acc + 1) key t

let key_to_int (key : string) (piano : piano) = acc_key_to_int 0 key piano

let rec tonality_to_indexes (key : int) (curr_index : int) = function
  | [] -> []
  | h :: t ->
      let new_index = (key + curr_index + h) mod 13 in
      new_index :: tonality_to_indexes 0 new_index t

let sorted_note_indexes (piano : piano) (scale : scale) : int list =
  scale.tonality
  |> tonality_to_indexes (key_to_int scale.key piano) 0
  |> List.sort compare

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
