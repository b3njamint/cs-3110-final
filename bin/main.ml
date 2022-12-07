open Music

let create_file_name (name : string) =
  "data" ^ Filename.dir_sep ^ name ^ ".json"

let piano_file = create_file_name "piano"
let tonalities_file = create_file_name "tonalities"
let segments_file = create_file_name "segments"
let piano = piano_from_json (Yojson.Basic.from_file piano_file)

let scale (name : string) (key : string) =
  scale_from_json (Yojson.Basic.from_file tonalities_file) name key

let random_beginning =
  segment_from_json (Yojson.Basic.from_file segments_file) true

let random_ending =
  segment_from_json (Yojson.Basic.from_file segments_file) false

(** [rec_get_valid_length length] continues to ask for [length] until it is
    valid. A length is valid if it is greater than or equal to 10. *)
let rec rec_get_valid_length (length : int) : int =
  if not (length >= 10) then (
    ANSITerminal.print_string [ ANSITerminal.red ]
      ("\nEntered length: " ^ string_of_int length ^ " is not valid length.\n");
    ANSITerminal.print_string [ ANSITerminal.blue ]
      "\nPlease enter length of melody (at least 10).\n";
    print_string "\n> ";
    match read_line () with
    | exception End_of_file -> rec_get_valid_length 0
    | entered_length -> (
        try rec_get_valid_length (int_of_string entered_length)
        with _ -> rec_get_valid_length ~-1))
  else length

(** [is_valid_key key lst] is true if [key] is an element in [lst] and is false 
    otherwise. *)
let rec is_valid_key (key : string) = function
  | [] -> false
  | h :: t -> if h = key then true else is_valid_key key t

(** [rec_get_valid_key key] continues to ask for [key] until it is
    valid. A valid key must cause [is_valid_key key] to be true. *)
let rec rec_get_valid_key (key : string) : string =
  if not (is_valid_key key piano) then (
    ANSITerminal.print_string [ ANSITerminal.red ]
      ("\nEntered key: " ^ key ^ " is not valid key.\n");
    ANSITerminal.print_string [ ANSITerminal.blue ]
      "\nPlease enter key of melody.\n";
    print_string "\n> ";
    match read_line () with
    | exception End_of_file -> rec_get_valid_key ""
    | entered_key ->
        entered_key |> String.trim |> String.uppercase_ascii
        |> rec_get_valid_key)
  else key

(** [is_valid_scale_name name key] is true if [scale name key] is [Some s] 
    (aka Some scale) and is false otherwise. *)
let rec is_valid_scale_name (name : string) (key : string) =
  match scale name key with None -> false | Some s -> true

(** [rec_get_valid_scale_name name key] continues to ask for [name] until it is
    valid. A valid name must cause [is_valid_scale_name name key] to be true. *)
let rec rec_get_valid_scale_name (name : string) (key : string) : string =
  if not (is_valid_scale_name name key) then (
    ANSITerminal.print_string [ ANSITerminal.red ]
      ("\nEntered tonality name: " ^ name ^ " is not valid tonality name.\n");
    ANSITerminal.print_string [ ANSITerminal.blue ]
      "\n\
       Please enter tonality of melody. Options: major; minor; minor_harmonic.\n";
    print_string "\n> ";
    match read_line () with
    | exception End_of_file -> rec_get_valid_scale_name "" key
    | entered_name -> rec_get_valid_scale_name entered_name key)
  else name

(** [get_valid_scale_name key] asks for a scale name (aka [entered_name]) and 
    calls [rec_get_valid_scale_name entered_name key]. *)
let get_valid_scale_name (key : string) =
  ANSITerminal.print_string [ ANSITerminal.blue ]
    "\n\
     Please enter tonality of melody. Options: major; minor; minor_harmonic.\n";
  print_string "\n> ";
  match read_line () with
  | exception End_of_file -> rec_get_valid_scale_name "" key
  | entered_name -> rec_get_valid_scale_name entered_name key

(** [get_valid_scale_name key] calls [get_valid_scale_name key] to get a 
    [scale_name] and then gets scale of [scale_name] by calling 
    [scale scale_name key]. *)
let get_valid_scale (key : string) =
  let scale_name = get_valid_scale_name key in
  match scale scale_name key with None -> exit 1 | Some s -> s

(** [print_music lst] prints the elements in [lst] with spaces in between and 
    then exits. *)
let rec print_music (delim : string) = function
  | [] -> print_endline "\n"
  | h :: t ->
      ANSITerminal.print_string [ ANSITerminal.yellow ] (h ^ delim);
      print_music delim t

(** [main] asks user for inputs in terminal and calls functions to create melody
    based on inputs. *)
let main () =
  ANSITerminal.print_string [ ANSITerminal.green ]
    "\n\nWelcome to the melody generator!\n";
  let key =
    ANSITerminal.print_string [ ANSITerminal.blue ]
      "\nPlease enter key of melody.\n";
    print_string "\n> ";
    match read_line () with
    | exception End_of_file -> rec_get_valid_key ""
    | entered_key ->
        entered_key |> String.trim |> String.uppercase_ascii
        |> rec_get_valid_key
  in
  let scale = get_valid_scale key in
  let length =
    ANSITerminal.print_string [ ANSITerminal.blue ]
      "\nPlease enter length of melody (at least 10).\n";
    print_string "\n> ";
    match read_line () with
    | exception End_of_file -> rec_get_valid_length 0
    | entered_length -> rec_get_valid_length (int_of_string entered_length)
  in
  let notes = create_notes piano scale in
  let mid_seg_len =
    length - List.length random_beginning - List.length random_ending
  in
  let mid_seed = generate_seed mid_seg_len (List.length notes) in
  let seed = random_beginning @ mid_seed @ random_ending in
  let melody = create_melody notes seed in
  ANSITerminal.print_string [ ANSITerminal.green ] "\nMelody: ";
  print_music " " melody;
  let chords = create_chords piano scale in
  let left_hand = create_left_hand melody chords seed in
  ANSITerminal.print_string [ ANSITerminal.green ] "Chords: ";
  print_music " " left_hand;
  let seed_print = List.map (fun e -> string_of_int e) seed in
  ANSITerminal.print_string [ ANSITerminal.green ] "Seed: ";
  print_music "" seed_print;
  exit 0

let () = main ()
