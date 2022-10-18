open Music

let create_file_name (name : string) = "data" ^ Filename.dir_sep ^ name ^ ".json"
let piano_file = create_file_name "piano"
let tonalities_file = create_file_name "tonalities"

let piano = piano_from_json (Yojson.Basic.from_file piano_file)
let scale (name: string) (key: string) = scale_from_json (Yojson.Basic.from_file piano_file) name key

let piano_length = List.length piano

let rec is_valid_key (key : string) = function
  | [] -> false
  | h :: t -> if (h = key) then true else is_valid_key key t

let rec rec_get_valid_key (key : string) : string =
  if not (is_valid_key key piano) then (
    print_endline ("\nEntered key: " ^ key ^ " is not valid key.\n");
    print_endline "Please enter key of melody.\n";
    print_string "> ";
    match read_line () with
    | exception End_of_file -> rec_get_valid_key ""
    | entered_key -> rec_get_valid_key entered_key)
  else key

let get_valid_key =
  print_endline "Please enter key of melody.\n";
  print_string "> ";
  match read_line () with
    | exception End_of_file -> rec_get_valid_key ""
    | entered_key -> rec_get_valid_key entered_key

let rec is_valid_scale_name (name : string) (key: string) = 
  match scale name key with
  | None -> false
  | Some s -> true

let rec rec_get_valid_scale_name (name : string) (key: string) : string =
  if not (is_valid_scale_name name key) then (
    print_endline ("\nEntered tonality name: " ^ name ^ " is not valid tonality name.\n");
    print_endline "Please enter tonality of melody.\n";
    print_string "> ";
    match read_line () with
    | exception End_of_file -> rec_get_valid_scale_name "" key
    | entered_name -> rec_get_valid_scale_name entered_name key)
  else name

let get_valid_scale_name (key: string) =
  print_endline "Please enter tonality of melody.\n";
  print_string "> ";
  match read_line () with
    | exception End_of_file -> rec_get_valid_scale_name "" key
    | entered_name -> rec_get_valid_scale_name entered_name key

let get_valid_scale (key: string) = 
  let scale_name = get_valid_scale_name key in match scale scale_name key with 
  | None -> exit 1
  | Some s -> s

let rec rec_get_valid_length (length : int) : int = 
  if not (length > 0) then (
    print_endline ("\nEntered length: " ^ string_of_int length ^ " is not valid length.\n");
    print_endline "Please enter length of melody.\n";
    print_string "> ";
    match read_line () with
    | exception End_of_file -> rec_get_valid_length 0
    | entered_length -> rec_get_valid_length (int_of_string entered_length))
  else length

let get_valid_length = 
  print_endline "Please enter length of melody.\n";
  print_string "> ";
  match read_line () with
    | exception End_of_file -> rec_get_valid_length 0
    | entered_length -> rec_get_valid_length (int_of_string entered_length)

let rec print_melody = function
| [] -> exit 0
  | h :: t -> print_string h; print_melody t

let play_melody = 
  let key = get_valid_key in
  let scale = get_valid_scale key in
  let length = get_valid_length in
  let seed = generate_seed piano_length length in 
  let notes = create_notes piano scale in
  let melody = create_melody notes seed in print_melody melody

(** [main ()] prompts for the game to play, then starts it. *)
let main () =
  ANSITerminal.print_string [ ANSITerminal.blue ]
    "\n\nWelcome to the melody generator.\n";
    play_melody

(* Execute the game engine. *)
let () = main ()
