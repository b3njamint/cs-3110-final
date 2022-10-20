(** Representation of melody generator. *)

type scale = { key : string; steps : int list }
type piano = string list
type notes = string list
type seed = int list

exception UnknownKey of string
exception BadIndex of int

val piano_from_json : Yojson.Basic.t -> piano
(** [piano_from_json j] is the notes that [j] contains. Requires: [j] is a valid JSON piano representation. *)

val scale_from_json : Yojson.Basic.t -> string -> string -> scale option
(** [scale_from_json j s k] is Some scale with key [k] and tonality with name [s] in the json file [j] which contains all the different tonalities, if tonality does not exist then None. Requires: [j] is a valid JSON tonalities representation. *)

val segment_from_json : Yojson.Basic.t -> bool -> seed
(** [segment_from_json j b] is the seed for a randomly selected pattern of notes that [j] contains. Returns starting pattern if [b] is true, otherwise returns ending pattern. Requires: [j] is a valid JSON segments representation. *)

val generate_seed : int -> int -> seed
(** [generate_seed l r] is a list of random indexes of length [l] from range [0..r - 1]. *)

val create_notes : piano -> scale -> notes
(** [create_notes p s] are the possible notes to randomly choose from based on scale [s] and piano [p]. *)

val create_melody : notes -> seed -> string list
(** [create_melody notes indexes] is the melody created from the notes and the list of random indexes. *)
