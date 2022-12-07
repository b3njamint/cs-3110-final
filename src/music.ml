open Yojson
open Yojson.Basic.Util

type scale = {
  key : string;
  steps : int list;
}

type tonality = {
  name : string;
  steps : int list;
}

type tonalities = { tonalities : tonality list }
type piano = string list
type notes = string list
type seed = int list

exception UnknownKey of string
exception BadIndex of int

let piano_from_json json =
  json |> member "piano" |> to_list |> List.map to_string

let tonality_of_json json =
  {
    name = json |> member "name" |> to_string;
    steps = json |> member "steps" |> to_list |> List.map to_int;
  }

let tonalities_from_json json =
  {
    tonalities =
      json |> member "tonalities" |> to_list |> List.map tonality_of_json;
  }

let scale_names json =
  List.fold_left
    (fun acc elt -> if List.mem elt.name acc then acc else elt.name :: acc)
    [] json.tonalities
  |> List.rev

let scale_from_json json scale key =
  let found_tonalities = tonalities_from_json json in
  let found_scale =
    List.find_opt (fun a -> a.name = scale) found_tonalities.tonalities
  in
  match found_scale with
  | None -> None
  | Some s -> Some { key; steps = s.steps }

let segment_from_json json is_beg : seed =
  Random.self_init ();
  if is_beg then
    let patterns = json |> member "beginnings" |> to_list |> List.map to_list in
    let ind = Random.int (List.length patterns - 1) in
    List.nth patterns ind |> List.map to_int
  else
    let patterns = json |> member "endings" |> to_list |> List.map to_list in
    let ind = Random.int (List.length patterns - 1) in
    List.nth patterns ind |> List.map to_int

(** [generate_seed_helper lst length range] is a list of [length] of random ints
    within [0..range-1]. *)
let rec generate_seed_helper lst length range : seed =
  if length = 0 then lst
  else
    let n = Random.int range in
    let lst1 = lst @ [ n ] in
    generate_seed_helper lst1 (length - 1) range

let generate_seed (length : int) (range : int) : seed =
  Random.self_init ();
  generate_seed_helper [] length range

(** [acc_key_to_int acc key lst] finds the index of the [key] in [lst]. *)
let rec acc_key_to_int (acc : int) (key : string) = function
  | [] -> raise (UnknownKey key)
  | h :: t -> if String.equal h key then acc else acc_key_to_int (acc + 1) key t

(** [key_to_int key piano] is the index of the [key] in [piano]. *)
let key_to_int (key : string) (piano : piano) = acc_key_to_int 0 key piano

(** [steps_to_indexes curr_index lst] is the list of note indexes based on [lst]
    aka the scale steps. *)
let rec steps_to_indexes (curr_index : int) = function
  | [] -> []
  | h :: t ->
      let new_index = (curr_index + h) mod 12 in
      new_index :: steps_to_indexes new_index t

(** [sorted_note_indexes piano scale] is the list of sorted note indexes based
    on the [scale]. *)
let sorted_note_indexes (piano : piano) (scale : scale) : int list =
  scale.steps
  |> steps_to_indexes (key_to_int scale.key piano)
  |> List.sort_uniq compare

(** [acc_create_notes curr_index piano sorted_note_indexes] is the list of notes
    that corresponds to the notes in [piano] at [sorted_note_indexes]. *)
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

let create_single_chord (index : int) (notes : notes) : string =
  "(" ^ List.nth notes 0 ^ "," ^ List.nth notes 2 ^ "," ^ List.nth notes 2 ^ ")"

let create_chords (piano : piano) (scale : scale) : string list =
  let notes = create_notes piano scale in
  let chord_I =
    "(" ^ List.nth notes 0 ^ "," ^ List.nth notes 2 ^ "," ^ List.nth notes 4
    ^ ")"
  in
  let chord_IV =
    "(" ^ List.nth notes 3 ^ "," ^ List.nth notes 5 ^ "," ^ List.nth notes 0
    ^ ")"
  in
  let chord_V =
    "(" ^ List.nth notes 4 ^ "," ^ List.nth notes 6 ^ "," ^ List.nth notes 1
    ^ ")"
  in
  [ chord_I; chord_IV; chord_V ]

(** [find_note notes index curr_index] is the note in [notes] at [index]. *)
let rec find_note (notes : notes) (index : int) (curr_index : int) : string =
  match notes with
  | [] -> raise (BadIndex index)
  | h :: t ->
      if index = curr_index then h else find_note t index (curr_index + 1)

let rec create_melody (notes : notes) (seed : seed) : string list =
  match seed with
  | [] -> []
  | h :: t -> find_note notes h 0 :: create_melody notes t

let rec create_left_hand (melody : string list) (chords : string list)
    (seed : seed) : string list =
  match (melody, seed) with
  | e1 :: e2 :: e3 :: e4 :: t, se :: st ->
      List.nth chords (se mod 3) :: create_left_hand t chords st
  | _ :: t, se :: st ->
      List.nth chords (se mod 3) :: create_left_hand t chords st
  | _ -> []
