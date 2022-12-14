open Yojson
open Yojson.Basic.Util
open Mm_audio
open Mm_ao

type scale = {
  key : string;
  steps : int list;
}

type tonality = {
  name : string;
  steps : int list;
}

type named_frequency = {
  name : string;
  frequency : float;
}

type octave =
  | SubContra of string
  | Contra of string
  | Great of string
  | Small of string
  | OneLine of string
  | TwoLine of string
  | ThreeLine of string
  | FourLine of string
  | FiveLine of string

type note =
  | C of string
  | Cs of string
  | D of string
  | Ds of string
  | E of string
  | F of string
  | Fs of string
  | G of string
  | Gs of string
  | A of string
  | As of string
  | B of string

type tonalities = { tonalities : tonality list }
type frequencies = { frequencies : named_frequency list }
type piano = string list
type notes = string list
type seed = int list

exception UnknownKey of string
exception BadIndex of int

let colors =
  [
    ANSITerminal.red;
    ANSITerminal.yellow;
    ANSITerminal.green;
    ANSITerminal.cyan;
    ANSITerminal.blue;
    ANSITerminal.magenta;
    ANSITerminal.black;
  ]

let create_file_name (name : string) =
  "data" ^ Filename.dir_sep ^ name ^ ".json"

let frequency_file = create_file_name "frequencies"

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

let frequency_of_json json =
  {
    name = json |> member "name" |> to_string;
    frequency = json |> member "frequency" |> to_float;
  }

let frequencies_from_json json =
  {
    frequencies =
      json |> member "frequencies" |> to_list |> List.map frequency_of_json;
  }

let scale_names json =
  List.fold_left
    (fun acc (elt : tonality) ->
      if List.mem elt.name acc then acc else elt.name :: acc)
    [] json.tonalities
  |> List.rev

let scale_from_json json scale key =
  let found_tonalities = tonalities_from_json json in
  let found_scale =
    List.find_opt
      (fun (a : tonality) -> a.name = scale)
      found_tonalities.tonalities
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

let frequency_from_name name =
  let parsed_frequencies =
    frequencies_from_json (Yojson.Basic.from_file frequency_file)
  in
  let found_frequency =
    List.find
      (fun (f : named_frequency) -> f.name = name)
      parsed_frequencies.frequencies
  in
  found_frequency.frequency

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

let rec reorder_notes (list : notes) (point : int) : notes =
  if point = 0 then list
  else
    match list with
    | [] -> []
    | hd :: tl -> reorder_notes (List.append tl [ hd ]) (point - 1)

let index_of_start e lst =
  let rec index_rec i = function
    | [] -> raise Not_found
    | hd :: tl -> if hd = e then i else index_rec (i + 1) tl
  in
  index_rec 0 lst

let create_notes (piano : piano) (scale : scale) : notes =
  let unsorted_notes =
    sorted_note_indexes piano scale |> acc_create_notes 0 piano
  in
  reorder_notes unsorted_notes (index_of_start scale.key unsorted_notes)

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

let rec acc_create_line (acc : string) (length : int) : string =
  match length with
  | 0 -> acc ^ "\n"
  | i -> "-" ^ acc_create_line acc (i - 1)

let rec acc_create_note_line (acc : string) (note : string)
    (melody : string list) : string =
  match melody with
  | [] -> acc ^ "\n"
  | h :: t ->
      let new_acc = acc ^ " " ^ if h == note then "♩" else " " in
      acc_create_note_line new_acc note t

let rec create_note_lines (acc : string) (notes : notes) (melody : string list)
    (colors : ANSITerminal.style list) : string =
  match (notes, colors) with
  | [], _ -> acc
  | _, [] -> acc
  | h1 :: t1, h2 :: t2 ->
      let note = h1 ^ if String.length h1 == 1 then " " else "" in
      let note_line = acc_create_note_line "" h1 melody in
      ANSITerminal.print_string [ h2 ] note;
      ANSITerminal.print_string [ ANSITerminal.Bold ] "|";
      ANSITerminal.print_string [ h2 ] note_line;
      create_note_lines (acc ^ note ^ "|" ^ note_line) t1 melody t2

let create_melody_note_sheet (notes : notes) (melody : string list) : string =
  let length = List.length melody in
  let line = acc_create_line "" (2 * (length + 2)) in
  ANSITerminal.print_string [ ANSITerminal.Bold ] ("\n" ^ line);
  let note_lines = create_note_lines "" notes melody colors in
  ANSITerminal.print_string [ ANSITerminal.Bold ] (line ^ "\n");
  line ^ note_lines ^ line

let activate_audio_player (frequency : float) (instrument : sounds) =
  let channels = 2 in
  let sample_rate = 44100 in
  let ao = new Mm_ao.writer channels sample_rate in
  let wav = new Audio.IO.Writer.to_wav_file channels sample_rate "out.wav" in
  let blen = 1024 in
  let buf = Audio.create channels blen in
  let sine =
    match instrument with
    | Sine ->
        new Audio.Generator.of_mono
          (new Audio.Mono.Generator.sine sample_rate frequency)
    | Square ->
        new Audio.Generator.of_mono
          (new Audio.Mono.Generator.square sample_rate frequency)
    | Saw ->
        new Audio.Generator.of_mono
          (new Audio.Mono.Generator.saw sample_rate frequency)
    | Triangle ->
        new Audio.Generator.of_mono
          (new Audio.Mono.Generator.triangle sample_rate frequency)
  in
  for _ = 0 to (sample_rate / blen) - 1 do
    sine#fill buf 0 blen;
    wav#write buf 0 blen;
    ao#write buf 0 blen
  done;
  wav#close;
  ao#close

let rec play_melody (melody : string list) (octave : string)
    (instrument : sounds) : unit =
  let oct =
    match octave with
    | "sub contra" -> "0"
    | "contra" -> "1"
    | "great" -> "2"
    | "small" -> "3"
    | "1 line" -> "4"
    | "2 line" -> "5"
    | "3 line" -> "6"
    | "4 line" -> "7"
    | "5 line" -> "8"
    | _ -> raise (UnknownKey octave)
  in
  match melody with
  | [] -> ()
  | h :: t ->
      activate_audio_player (frequency_from_name (h ^ oct)) instrument;
      (* (let note = match h with | "C" -> C "C" | "C#" -> Cs "C#" | "D" -> D
         "D" | "D#" -> Ds "D#" | "E" -> E "E" | "F" -> F "F" | "F#" -> Fs "F#" |
         "G" -> G "G" | "G#" -> Gs "G#" | "A" -> A "A" | "A#" -> As "A#" | "B"
         -> B "B" | _ -> raise (UnknownKey h) in *)
      (* match (note, oct) with *)
      (* | C, SubContra -> activate_audio_player (frequency_from_name "C0") |
         Cs, SubContra -> activate_audio_player (frequency_from_name "C#0") | D,
         SubContra -> activate_audio_player (frequency_from_name "D0") | Ds,
         SubContra -> activate_audio_player (frequency_from_name "D#0") | E,
         SubContra -> activate_audio_player (frequency_from_name "E0") | F,
         SubContra -> activate_audio_player (frequency_from_name "F0") | Fs,
         SubContra -> activate_audio_player (frequency_from_name "F#0") | G,
         SubContra -> activate_audio_player (frequency_from_name "G0") | Gs,
         SubContra -> activate_audio_player (frequency_from_name "G#0") | A,
         SubContra -> activate_audio_player (frequency_from_name "A0") | As,
         SubContra -> activate_audio_player (frequency_from_name "A#0") | B,
         SubContra -> activate_audio_player (frequency_from_name "B0") | C,
         Contra -> activate_audio_player (frequency_from_name "C1") | Cs, Contra
         -> activate_audio_player (frequency_from_name "C#1") | D, Contra ->
         activate_audio_player (frequency_from_name "D1") | Ds, Contra ->
         activate_audio_player (frequency_from_name "D#1") | E, Contra ->
         activate_audio_player (frequency_from_name "E1") | F, Contra ->
         activate_audio_player (frequency_from_name "F1") | Fs, Contra ->
         activate_audio_player (frequency_from_name "F#1") | G, Contra ->
         activate_audio_player (frequency_from_name "G1") | Gs, Contra ->
         activate_audio_player (frequency_from_name "G#1") | A, Contra ->
         activate_audio_player (frequency_from_name "A1") | As, Contra ->
         activate_audio_player (frequency_from_name "A#1") | B, Contra ->
         activate_audio_player (frequency_from_name "B1") | C, Great ->
         activate_audio_player (frequency_from_name "C2") | Cs, Great ->
         activate_audio_player (frequency_from_name "C#2") | D, Great ->
         activate_audio_player (frequency_from_name "D2") | Ds, Great ->
         activate_audio_player (frequency_from_name "D#2") | E, Great ->
         activate_audio_player (frequency_from_name "E2") | F, Great ->
         activate_audio_player (frequency_from_name "F2") | Fs, Great ->
         activate_audio_player (frequency_from_name "F#2") | G, Great ->
         activate_audio_player (frequency_from_name "G2") | Gs, Great ->
         activate_audio_player (frequency_from_name "G#2") | A, Great ->
         activate_audio_player (frequency_from_name "A2") | As, Great ->
         activate_audio_player (frequency_from_name "A#2") | B, Great ->
         activate_audio_player (frequency_from_name "B2") | C, Small ->
         activate_audio_player (frequency_from_name "C3") | Cs, Small ->
         activate_audio_player (frequency_from_name "C#3") | D, Small ->
         activate_audio_player (frequency_from_name "D3") | Ds, Small ->
         activate_audio_player (frequency_from_name "D#3") | E, Small ->
         activate_audio_player (frequency_from_name "E3") | F, Small ->
         activate_audio_player (frequency_from_name "F3") | Fs, Small ->
         activate_audio_player (frequency_from_name "F#3") | G, Small ->
         activate_audio_player (frequency_from_name "G3") | Gs, Small ->
         activate_audio_player (frequency_from_name "G#3") | A, Small ->
         activate_audio_player (frequency_from_name "A3") | As, Small ->
         activate_audio_player (frequency_from_name "A#3") | B, Small ->
         activate_audio_player (frequency_from_name "B3") | C, OneLine ->
         activate_audio_player (frequency_from_name "C4") | Cs, OneLine ->
         activate_audio_player (frequency_from_name "C#4") | D, OneLine ->
         activate_audio_player (frequency_from_name "D4") | Ds, OneLine ->
         activate_audio_player (frequency_from_name "D#4") | E, OneLine ->
         activate_audio_player (frequency_from_name "E4") | F, OneLine ->
         activate_audio_player (frequency_from_name "F4") | Fs, OneLine ->
         activate_audio_player (frequency_from_name "F#4") | G, OneLine ->
         activate_audio_player (frequency_from_name "G4") | Gs, OneLine ->
         activate_audio_player (frequency_from_name "G#4") | A, OneLine ->
         activate_audio_player (frequency_from_name "A4") | As, OneLine ->
         activate_audio_player (frequency_from_name "A#4") | B, OneLine ->
         activate_audio_player (frequency_from_name "B4") | C, TwoLine ->
         activate_audio_player (frequency_from_name "C5") | Cs, TwoLine ->
         activate_audio_player (frequency_from_name "C#5") | D, TwoLine ->
         activate_audio_player (frequency_from_name "D5") | Ds, TwoLine ->
         activate_audio_player (frequency_from_name "D#5") | E, TwoLine ->
         activate_audio_player (frequency_from_name "E5") | F, TwoLine ->
         activate_audio_player (frequency_from_name "F5") | Fs, TwoLine ->
         activate_audio_player (frequency_from_name "F#5") | G, TwoLine ->
         activate_audio_player (frequency_from_name "G5") | Gs, TwoLine ->
         activate_audio_player (frequency_from_name "G#5") | A, TwoLine ->
         activate_audio_player (frequency_from_name "A5") | As, TwoLine ->
         activate_audio_player (frequency_from_name "A#5") | B, TwoLine ->
         activate_audio_player (frequency_from_name "B5") | C, ThreeLine ->
         activate_audio_player (frequency_from_name "C6") | Cs, ThreeLine ->
         activate_audio_player (frequency_from_name "C#6") | D, ThreeLine ->
         activate_audio_player (frequency_from_name "D6") | Ds, ThreeLine ->
         activate_audio_player (frequency_from_name "D#6") | E, ThreeLine ->
         activate_audio_player (frequency_from_name "E6") | F, ThreeLine ->
         activate_audio_player (frequency_from_name "F6") | Fs, ThreeLine ->
         activate_audio_player (frequency_from_name "F#6") | G, ThreeLine ->
         activate_audio_player (frequency_from_name "G6") | Gs, ThreeLine ->
         activate_audio_player (frequency_from_name "G#6") | A, ThreeLine ->
         activate_audio_player (frequency_from_name "A6") | As, ThreeLine ->
         activate_audio_player (frequency_from_name "A#6") | B, ThreeLine ->
         activate_audio_player (frequency_from_name "B6") | C, FourLine ->
         activate_audio_player (frequency_from_name "C7") | Cs, FourLine ->
         activate_audio_player (frequency_from_name "C#7") | D, FourLine ->
         activate_audio_player (frequency_from_name "D7") | Ds, FourLine ->
         activate_audio_player (frequency_from_name "D#7") | E, FourLine ->
         activate_audio_player (frequency_from_name "E7") | F, FourLine ->
         activate_audio_player (frequency_from_name "F7") | Fs, FourLine ->
         activate_audio_player (frequency_from_name "F#7") | G, FourLine ->
         activate_audio_player (frequency_from_name "G7") | Gs, FourLine ->
         activate_audio_player (frequency_from_name "G#7") | A, FourLine ->
         activate_audio_player (frequency_from_name "A7") | As, FourLine ->
         activate_audio_player (frequency_from_name "A#7") | B, FourLine ->
         activate_audio_player (frequency_from_name "B7") | C, FiveLine ->
         activate_audio_player (frequency_from_name "C8") | Cs, FiveLine ->
         activate_audio_player (frequency_from_name "C#8") | D, FiveLine ->
         activate_audio_player (frequency_from_name "D8") | Ds, FiveLine ->
         activate_audio_player (frequency_from_name "D#8") | E, FiveLine ->
         activate_audio_player (frequency_from_name "E8") | F, FiveLine ->
         activate_audio_player (frequency_from_name "F8") | Fs, FiveLine ->
         activate_audio_player (frequency_from_name "F#8") | G, FiveLine ->
         activate_audio_player (frequency_from_name "G8") | Gs, FiveLine ->
         activate_audio_player (frequency_from_name "G#8") | A, FiveLine ->
         activate_audio_player (frequency_from_name "A8") | As, FiveLine ->
         activate_audio_player (frequency_from_name "A#8") | B, FiveLine ->
         activate_audio_player (frequency_from_name "B8")); *)
      play_melody t octave
