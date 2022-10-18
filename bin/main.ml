open Music

let create_file_name (name : string) =
  "data" ^ Filename.dir_sep ^ name ^ ".json"

let piano_file = create_file_name "piano"
let tonalities_file = create_file_name "tonalities"
let piano = piano_from_json (Yojson.Basic.from_file piano_file)

let scale (name : string) (key : string) =
  scale_from_json (Yojson.Basic.from_file tonalities_file) name key

let rec rec_get_valid_length (length : int) : int =
  if not (length > 0) then (
    ANSITerminal.print_string [ ANSITerminal.red ]
      ("\nEntered length: " ^ string_of_int length ^ " is not valid length.\n");
    ANSITerminal.print_string [ ANSITerminal.blue ]
      "\nPlease enter length of melody.\n";
    print_string "\n> ";
    match read_line () with
    | exception End_of_file -> rec_get_valid_length 0
    | entered_length -> rec_get_valid_length (int_of_string entered_length))
  else length

let rec is_valid_key (key : string) = function
  | [] -> false
  | h :: t -> if h = key then true else is_valid_key key t

let rec rec_get_valid_key (key : string) : string =
  if not (is_valid_key key piano) then (
    ANSITerminal.print_string [ ANSITerminal.red ]
      ("\nEntered key: " ^ key ^ " is not valid key.\n");
    ANSITerminal.print_string [ ANSITerminal.blue ]
      "\nPlease enter key of melody.\n";
    print_string "\n> ";
    match read_line () with
    | exception End_of_file -> rec_get_valid_key ""
    | entered_key -> rec_get_valid_key entered_key)
  else key

let rec is_valid_scale_name (name : string) (key : string) =
  match scale name key with None -> false | Some s -> true

let rec rec_get_valid_scale_name (name : string) (key : string) : string =
  if not (is_valid_scale_name name key) then (
    ANSITerminal.print_string [ ANSITerminal.red ]
      ("\nEntered tonality name: " ^ name ^ " is not valid tonality name.\n");
    ANSITerminal.print_string [ ANSITerminal.blue ]
      "\nPlease enter tonality of melody.\n";
    print_string "\n> ";
    match read_line () with
    | exception End_of_file -> rec_get_valid_scale_name "" key
    | entered_name -> rec_get_valid_scale_name entered_name key)
  else name

let get_valid_scale_name (key : string) =
  ANSITerminal.print_string [ ANSITerminal.blue ]
    "\nPlease enter tonality of melody.\n";
  print_string "\n> ";
  match read_line () with
  | exception End_of_file -> rec_get_valid_scale_name "" key
  | entered_name -> rec_get_valid_scale_name entered_name key

let get_valid_scale (key : string) =
  let scale_name = get_valid_scale_name key in
  match scale scale_name key with None -> exit 1 | Some s -> s

let rec print_melody = function
  | [] ->
      print_endline "\n";
      exit 0
  | h :: t ->
      ANSITerminal.print_string [ ANSITerminal.green ] (h ^ " ");
      print_melody t

let main () =
  ANSITerminal.print_string [ ANSITerminal.green ]
    "\n\nWelcome to the melody generator!\n";
  let key =
    ANSITerminal.print_string [ ANSITerminal.blue ]
      "\nPlease enter key of melody.\n";
    print_string "\n> ";
    match read_line () with
    | exception End_of_file -> rec_get_valid_key ""
    | entered_key -> rec_get_valid_key entered_key
  in
  let scale = get_valid_scale key in
  let length =
    ANSITerminal.print_string [ ANSITerminal.blue ]
      "\nPlease enter length of melody.\n";
    print_string "\n> ";
    match read_line () with
    | exception End_of_file -> rec_get_valid_length 0
    | entered_length -> rec_get_valid_length (int_of_string entered_length)
  in
  let notes = create_notes piano scale in
  let seed = generate_seed length (List.length notes) in
  let melody = create_melody notes seed in
  ANSITerminal.print_string [ ANSITerminal.green ] "\nMelody: ";
  print_melody melody

let () = main ()
