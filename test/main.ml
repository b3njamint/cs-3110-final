open OUnit2
open Music

let piano = [ "C"; "C#"; "D"; "D#"; "E"; "F"; "F#"; "G"; "G#"; "A"; "A#"; "B" ]
let major = [ 0; 2; 2; 1; 2; 2; 2; 1 ]
let minor = [ 0; 2; 1; 2; 2; 1; 2; 2 ]

(** [pp_string s] pretty-prints string [s]. *)
let pp_string s = "\"" ^ s ^ "\""

(** [pp_list pp_elt lst] pretty-prints list [lst], using [pp_elt] to
    pretty-print each element of [lst]. *)
let pp_list pp_elt lst =
  let pp_elts lst =
    let rec loop n acc = function
      | [] -> acc
      | [ h ] -> acc ^ pp_elt h
      | h1 :: (h2 :: t as t') ->
          if n = 100 then acc ^ "..." (* stop printing long list *)
          else loop (n + 1) (acc ^ pp_elt h1 ^ "; ") t'
    in
    loop 0 "" lst
  in
  "[" ^ pp_elts lst ^ "]"

let data_dir_prefix = "data" ^ Filename.dir_sep
let ton = Yojson.Basic.from_file (data_dir_prefix ^ "tonalities.json")

(** [scale_from_json_test name json scale key expected_output] constructs an
    OUnit test named [name] that asserts the quality of [expected_output] with
    [scale_from_json json scale key]. *)
let scale_from_json_test (name : string) (json : Yojson.Basic.t)
    (scale : string) (key : string) (expected_output : scale option) : test =
  name >:: fun _ ->
  assert_equal expected_output (scale_from_json json scale key)

(** [create_notes_test name input_piano input_scale] constructs an OUnit test
    named [name] that asserts the quality of [expected_output] with
    [create_notes input_piano input_scale]. *)
let create_notes_test (name : string) (input_piano : piano)
    (input_scale : scale) (expected_output : string list) : test =
  name >:: fun _ ->
  assert_equal expected_output
    (create_notes input_piano input_scale)
    ~printer:(pp_list pp_string)

let reorder_notes_test (name : string) (input_piano : piano)
    (input_scale : scale) (input_point : int) (expected_output : string list) :
    test =
  name >:: fun _ ->
  assert_equal expected_output
    (reorder_notes (create_notes input_piano input_scale) input_point)
    ~printer:(pp_list pp_string)

(** [generate_seed_test_randomness name length range] constructs an OUnit test
    named [name] that asserts that two generated seeds are not equivalent if the
    inputs are the same *)
let generate_seed_test_randomness (name : string) (length : int) (range : int) :
    test =
  name >:: fun _ ->
  assert (generate_seed length range != generate_seed length range)

(** [generate_seed_test_in_range name length range] contructs an OUnit test
    named [name] that asserts that the generated seeds are in the range
    specified *)
let generate_seed_test_in_range (name : string) (length : int) (range : int) :
    test =
  name >:: fun _ ->
  let rec check_range lst =
    match lst with
    | [] -> assert true
    | h :: t -> if h < range then check_range t else assert false
  in
  check_range (generate_seed length range)

(** [create_melody_test name input_notes input_seed] constructs an OUnit test
    named [name] that asserts the quality of [expected_output] with
    [create_melody input_notes input_seed]. *)
let create_melody_test (name : string) (input_notes : notes) (input_seed : seed)
    (expected_output : string list) : test =
  name >:: fun _ ->
  assert_equal expected_output
    (create_melody input_notes input_seed)
    ~printer:(pp_list pp_string)

(** [create_chords_test name piano scale expected_output] constructs an OUnit
    test named [name] that asserts the quality of [expected_output] with
    [create_chords piano scale]. *)
let create_chords_test (name : string) (input_piano : piano)
    (input_scale : scale) (expected_output : string list) : test =
  name >:: fun _ ->
  assert_equal expected_output
    (create_chords input_piano input_scale)
    ~printer:(pp_list pp_string)

(** [create_left_hand_test name melody chords seed expected_output] constructs
    an OUnit test named [name] that asserts the quality of [expected_output]
    with [create_left_hand melody chords seed]. *)
let create_left_hand_test (name : string) (melody : string list)
    (chords : string list) (seed : int list) (expected_output : string list) :
    test =
  name >:: fun _ ->
  assert_equal expected_output
    (create_left_hand melody chords seed)
    ~printer:(pp_list pp_string)

(** [create_melody_note_sheet_test name notes melody expected_output] constructs
    an OUnit test named [name] that asserts the quality of [expected_output]
    with [create_melody_note_sheet notes melody]. *)
let create_melody_note_sheet_test (name : string) (notes : notes)
    (melody : string list) (expected_output : string) : test =
  name >:: fun _ ->
  assert_equal expected_output
    (create_melody_note_sheet notes melody)
    ~printer:pp_string

let music_json_tests =
  [
    scale_from_json_test "test c major" ton "major" "C"
      (Some { key = "C"; steps = [ 0; 2; 2; 1; 2; 2; 2; 1 ] });
    scale_from_json_test "test a minor" ton "minor" "A"
      (Some { key = "A"; steps = [ 0; 2; 1; 2; 2; 1; 2; 2 ] });
  ]

let music_tests =
  [
    create_notes_test "c major" piano
      { key = "C"; steps = major }
      [ "C"; "D"; "E"; "F"; "G"; "A"; "B" ];
    create_notes_test "g major" piano
      { key = "G"; steps = major }
      [ "G"; "A"; "B"; "C"; "D"; "E"; "F#" ];
    create_notes_test "d major" piano
      { key = "D"; steps = major }
      [ "D"; "E"; "F#"; "G"; "A"; "B"; "C#" ];
    create_notes_test "a major" piano
      { key = "A"; steps = major }
      [ "A"; "B"; "C#"; "D"; "E"; "F#"; "G#" ];
    create_notes_test "e major" piano
      { key = "E"; steps = major }
      [ "E"; "F#"; "G#"; "A"; "B"; "C#"; "D#" ];
    create_notes_test "b major" piano
      { key = "B"; steps = major }
      [ "B"; "C#"; "D#"; "E"; "F#"; "G#"; "A#" ];
    create_notes_test "f major" piano
      { key = "F"; steps = major }
      [ "F"; "G"; "A"; "A#"; "C"; "D"; "E" ];
    create_notes_test "a minor" piano
      { key = "A"; steps = minor }
      [ "A"; "B"; "C"; "D"; "E"; "F"; "G" ];
    create_notes_test "e minor" piano
      { key = "E"; steps = minor }
      [ "E"; "F#"; "G"; "A"; "B"; "C"; "D" ];
    create_notes_test "b minor" piano
      { key = "B"; steps = minor }
      [ "B"; "C#"; "D"; "E"; "F#"; "G"; "A" ];
    create_notes_test "d minor" piano
      { key = "D"; steps = minor }
      [ "D"; "E"; "F"; "G"; "A"; "A#"; "C" ];
    create_notes_test "g minor" piano
      { key = "G"; steps = minor }
      [ "G"; "A"; "A#"; "C"; "D"; "D#"; "F" ];
    create_notes_test "c minor" piano
      { key = "C"; steps = minor }
      [ "C"; "D"; "D#"; "F"; "G"; "G#"; "A#" ];
    create_notes_test "f minor" piano
      { key = "F"; steps = minor }
      [ "F"; "G"; "G#"; "A#"; "C"; "C#"; "D#" ];
    create_melody_test "basic c major melody"
      [ "C"; "D"; "E"; "F"; "G"; "A"; "B" ]
      [
        0;
        1;
        2;
        3;
        4;
        5;
        6;
        0;
        1;
        2;
        3;
        4;
        5;
        6;
        0;
        1;
        2;
        3;
        4;
        5;
        6;
        0;
        1;
        2;
        3;
        4;
        5;
        6;
      ]
      [
        "C";
        "D";
        "E";
        "F";
        "G";
        "A";
        "B";
        "C";
        "D";
        "E";
        "F";
        "G";
        "A";
        "B";
        "C";
        "D";
        "E";
        "F";
        "G";
        "A";
        "B";
        "C";
        "D";
        "E";
        "F";
        "G";
        "A";
        "B";
      ];
    create_melody_test "random c major melody"
      [ "C"; "D"; "E"; "F"; "G"; "A"; "B" ]
      [
        6;
        4;
        3;
        5;
        1;
        3;
        0;
        2;
        3;
        3;
        4;
        6;
        1;
        5;
        4;
        2;
        0;
        1;
        3;
        6;
        6;
        3;
        1;
        4;
        0;
        0;
      ]
      [
        "B";
        "G";
        "F";
        "A";
        "D";
        "F";
        "C";
        "E";
        "F";
        "F";
        "G";
        "B";
        "D";
        "A";
        "G";
        "E";
        "C";
        "D";
        "F";
        "B";
        "B";
        "F";
        "D";
        "G";
        "C";
        "C";
      ];
    create_melody_test "basic b minor melody"
      [ "C#"; "D"; "E"; "F#"; "G"; "A"; "B" ]
      [
        0;
        1;
        2;
        3;
        4;
        5;
        6;
        0;
        1;
        2;
        3;
        4;
        5;
        6;
        0;
        1;
        2;
        3;
        4;
        5;
        6;
        0;
        1;
        2;
        3;
        4;
        5;
        6;
      ]
      [
        "C#";
        "D";
        "E";
        "F#";
        "G";
        "A";
        "B";
        "C#";
        "D";
        "E";
        "F#";
        "G";
        "A";
        "B";
        "C#";
        "D";
        "E";
        "F#";
        "G";
        "A";
        "B";
        "C#";
        "D";
        "E";
        "F#";
        "G";
        "A";
        "B";
      ];
    create_melody_test "random b minor melody"
      [ "C#"; "D"; "E"; "F#"; "G"; "A"; "B" ]
      [
        3;
        4;
        6;
        5;
        1;
        2;
        0;
        0;
        1;
        3;
        5;
        6;
        1;
        3;
        6;
        0;
        3;
        6;
        4;
        4;
        2;
        5;
        4;
        2;
        0;
      ]
      [
        "F#";
        "G";
        "B";
        "A";
        "D";
        "E";
        "C#";
        "C#";
        "D";
        "F#";
        "A";
        "B";
        "D";
        "F#";
        "B";
        "C#";
        "F#";
        "B";
        "G";
        "G";
        "E";
        "A";
        "G";
        "E";
        "C#";
      ];
    generate_seed_test_randomness "Randomness of seed: Length: 10, range: 10" 10
      10;
    generate_seed_test_randomness "Randomness of seed: Length: 10, range: 10" 10
      10;
    generate_seed_test_randomness "Randomness of seed: Length: 100, range: 9"
      100 9;
    generate_seed_test_randomness "Randomness of seed: Length: 50, range: 100"
      50 100;
    generate_seed_test_in_range "In range seeds: Length: 5, range: 7" 5 7;
    generate_seed_test_in_range "In range seeds: Length: 6, range: 100" 6 100;
    generate_seed_test_in_range "In range seeds: Length: 9, range: 50" 6 50;
    generate_seed_test_in_range "In range seeds: Length: 20, range: 15" 20 15;
    create_chords_test "chords for random C major melody" piano
      { key = "C"; steps = major }
      [ "(C,E,G)"; "(F,A,C)"; "(G,B,D)" ];
    create_chords_test "chords for random D major melody" piano
      { key = "D"; steps = major }
      [ "(D,F#,A)"; "(G,B,D)"; "(A,C#,E)" ];
    create_chords_test "chords for random A minor melody" piano
      { key = "A"; steps = minor }
      [ "(A,C,E)"; "(D,F,A)"; "(E,G,B)" ];
    create_chords_test "chords for random D# minor melody" piano
      { key = "D#"; steps = minor }
      [ "(D#,F#,A#)"; "(G#,B,D#)"; "(A#,C#,F)" ];
    create_chords_test "chords for random F major melody" piano
      { key = "F"; steps = major }
      [ "(F,A,C)"; "(A#,D,F)"; "(C,E,G)" ];
    create_chords_test "chords for random G major melody" piano
      { key = "G"; steps = major }
      [ "(G,B,D)"; "(C,E,G)"; "(D,F#,A)" ];
    reorder_notes_test "reorder d major" piano
      { key = "D"; steps = major }
      (index_of_start "D" (create_notes piano { key = "D"; steps = major }))
      [ "D"; "E"; "F#"; "G"; "A"; "B"; "C#" ];
    reorder_notes_test "reorder g major" piano
      { key = "G"; steps = major }
      (index_of_start "G" (create_notes piano { key = "G"; steps = major }))
      [ "G"; "A"; "B"; "C"; "D"; "E"; "F#" ];
    reorder_notes_test "reorder a minor" piano
      { key = "A"; steps = minor }
      (index_of_start "A" (create_notes piano { key = "A"; steps = minor }))
      [ "A"; "B"; "C"; "D"; "E"; "F"; "G" ];
    reorder_notes_test "reorder a major" piano
      { key = "A"; steps = major }
      (index_of_start "A" (create_notes piano { key = "A"; steps = major }))
      [ "A"; "B"; "C#"; "D"; "E"; "F#"; "G#" ];
    create_melody_note_sheet_test
      "create note sheet of c major melody length 10"
      [ "C"; "D"; "E"; "F"; "G"; "A"; "B" ]
      [ "C"; "E"; "G"; "E"; "C"; "C"; "E"; "F"; "D"; "C" ]
      "------------------------\n\
       C | ♩       ♩ ♩       ♩\n\
       D |                 ♩  \n\
       E |   ♩   ♩     ♩      \n\
       F |               ♩    \n\
       G |     ♩              \n\
       A |                    \n\
       B |                    \n\
       ------------------------\n";
    create_melody_note_sheet_test
      "create note sheet of a minor melody length 10"
      [ "C"; "D"; "E"; "F"; "G"; "A"; "B" ]
      [ "C"; "D"; "E"; "F"; "F"; "B"; "E"; "C"; "B"; "C" ]
      "------------------------\n\
       C | ♩             ♩   ♩\n\
       D |   ♩                \n\
       E |     ♩       ♩      \n\
       F |       ♩ ♩          \n\
       G |                    \n\
       A |                    \n\
       B |           ♩     ♩  \n\
       ------------------------\n";
    create_melody_note_sheet_test
      "create note sheet of e minor harmonic melody length 10"
      [ "C"; "D#"; "E"; "F#"; "G"; "A"; "B" ]
      [ "C"; "E"; "G"; "C"; "F#"; "D#"; "E"; "C"; "D#"; "C" ]
      "------------------------\n\
       C | ♩     ♩       ♩   ♩\n\
       D#|           ♩     ♩  \n\
       E |   ♩         ♩      \n\
       F#|         ♩          \n\
       G |     ♩              \n\
       A |                    \n\
       B |                    \n\
       ------------------------\n";
    create_melody_note_sheet_test
      "create note sheet of c major melody length 20"
      [ "C"; "D"; "E"; "F"; "G"; "A"; "B" ]
      [
        "C";
        "E";
        "G";
        "E";
        "B";
        "F";
        "D";
        "D";
        "C";
        "A";
        "F";
        "F";
        "E";
        "E";
        "F";
        "F";
        "F";
        "D";
        "E";
        "C";
      ]
      "--------------------------------------------\n\
       C | ♩               ♩                     ♩\n\
       D |             ♩ ♩                   ♩    \n\
       E |   ♩   ♩                 ♩ ♩         ♩  \n\
       F |           ♩         ♩ ♩     ♩ ♩ ♩      \n\
       G |     ♩                                  \n\
       A |                   ♩                    \n\
       B |         ♩                              \n\
       --------------------------------------------\n";
    create_melody_note_sheet_test
      "create note sheet of a minor melody length 20"
      [ "C"; "D"; "E"; "F"; "G"; "A"; "B" ]
      [
        "C";
        "E";
        "G";
        "E";
        "D";
        "B";
        "G";
        "A";
        "F";
        "F";
        "B";
        "D";
        "D";
        "G";
        "F";
        "B";
        "D";
        "E";
        "D";
        "C";
      ]
      "--------------------------------------------\n\
       C | ♩                                     ♩\n\
       D |         ♩             ♩ ♩       ♩   ♩  \n\
       E |   ♩   ♩                           ♩    \n\
       F |                 ♩ ♩         ♩          \n\
       G |     ♩       ♩             ♩            \n\
       A |               ♩                        \n\
       B |           ♩         ♩         ♩        \n\
       --------------------------------------------\n";
    create_melody_note_sheet_test
      "create note sheet of e minor harmonic melody length 20"
      [ "C"; "D#"; "E"; "F#"; "G"; "A"; "B" ]
      [
        "C";
        "E";
        "D#";
        "F#";
        "B";
        "A";
        "G";
        "B";
        "D#";
        "A";
        "A";
        "E";
        "A";
        "C";
        "E";
        "E";
        "F#";
        "D#";
        "E";
        "C";
      ]
      "--------------------------------------------\n\
       C | ♩                         ♩           ♩\n\
       D#|     ♩           ♩                 ♩    \n\
       E |   ♩                   ♩     ♩ ♩     ♩  \n\
       F#|       ♩                         ♩      \n\
       G |             ♩                          \n\
       A |           ♩       ♩ ♩   ♩              \n\
       B |         ♩     ♩                        \n\
       --------------------------------------------\n";
    create_melody_note_sheet_test
      "create note sheet of c major melody length 50"
      [ "C"; "D"; "E"; "F"; "G"; "A"; "B" ]
      [
        "C";
        "E";
        "D";
        "D";
        "F";
        "G";
        "G";
        "B";
        "D";
        "A";
        "D";
        "D";
        "B";
        "C";
        "B";
        "D";
        "D";
        "D";
        "G";
        "D";
        "F";
        "E";
        "C";
        "C";
        "B";
        "B";
        "D";
        "G";
        "F";
        "C";
        "G";
        "B";
        "E";
        "G";
        "F";
        "C";
        "A";
        "G";
        "C";
        "A";
        "B";
        "D";
        "G";
        "A";
        "C";
        "A";
        "F";
        "D";
        "E";
        "C";
      ]
      "--------------------------------------------------------------------------------------------------------\n\
       C | ♩                         ♩                 ♩ ♩           \
       ♩           ♩     ♩           ♩         ♩\n\
       D |     ♩ ♩         ♩   ♩ ♩       ♩ ♩ ♩   ♩             \
       ♩                             ♩           ♩    \n\
       E |   ♩                                       ♩                     \
       ♩                               ♩  \n\
       F |         ♩                               ♩               ♩           \
       ♩                       ♩      \n\
       G |           ♩ ♩                       ♩                 ♩     ♩     \
       ♩       ♩         ♩              \n\
       A |                   \
       ♩                                                     ♩     ♩       ♩   \
       ♩        \n\
       B |               ♩         ♩   ♩                   ♩ ♩           \
       ♩                 ♩                  \n\
       --------------------------------------------------------------------------------------------------------\n";
    create_melody_note_sheet_test
      "create note sheet of a minor melody length 50"
      [ "C"; "D"; "E"; "F"; "G"; "A"; "B" ]
      [
        "C";
        "D";
        "E";
        "F";
        "B";
        "C";
        "A";
        "F";
        "G";
        "E";
        "E";
        "C";
        "D";
        "F";
        "F";
        "B";
        "C";
        "D";
        "B";
        "B";
        "F";
        "D";
        "B";
        "A";
        "F";
        "E";
        "A";
        "C";
        "D";
        "A";
        "D";
        "A";
        "B";
        "B";
        "C";
        "A";
        "C";
        "F";
        "D";
        "E";
        "D";
        "D";
        "G";
        "A";
        "D";
        "A";
        "F";
        "E";
        "D";
        "C";
      ]
      "--------------------------------------------------------------------------------------------------------\n\
       C | ♩         ♩           ♩         ♩                     ♩             \
       ♩   ♩                         ♩\n\
       D |   ♩                     ♩         ♩       ♩             ♩   \
       ♩               ♩   ♩ ♩     ♩       ♩  \n\
       E |     ♩             ♩ ♩                             \
       ♩                           ♩               ♩    \n\
       F |       ♩       ♩           ♩ ♩           ♩       \
       ♩                         ♩                 ♩      \n\
       G |                 \
       ♩                                                                   \
       ♩              \n\
       A |             ♩                                 ♩     ♩     ♩   \
       ♩       ♩               ♩   ♩        \n\
       B |         ♩                     ♩     ♩ ♩     ♩                   ♩ \
       ♩                                \n\
       --------------------------------------------------------------------------------------------------------\n";
    create_melody_note_sheet_test
      "create note sheet of e minor harmonic melody length 50"
      [ "C"; "D#"; "E"; "F#"; "G"; "A"; "B" ]
      [
        "C";
        "G";
        "A";
        "A";
        "G";
        "G";
        "A";
        "C";
        "G";
        "A";
        "F#";
        "C";
        "A";
        "F#";
        "D#";
        "E";
        "E";
        "A";
        "F#";
        "F#";
        "D#";
        "D#";
        "F#";
        "A";
        "F#";
        "F#";
        "F#";
        "A";
        "G";
        "C";
        "B";
        "F#";
        "E";
        "D#";
        "B";
        "A";
        "A";
        "G";
        "F#";
        "F#";
        "C";
        "A";
        "D#";
        "C";
        "A";
        "C";
        "E";
        "F#";
        "D#";
        "C";
      ]
      "--------------------------------------------------------------------------------------------------------\n\
       C | ♩             ♩       ♩                                   \
       ♩                     ♩     ♩   ♩       ♩\n\
       D#|                             ♩           ♩ ♩                       \
       ♩                 ♩           ♩  \n\
       E |                               ♩ ♩                               \
       ♩                           ♩      \n\
       F#|                     ♩     ♩         ♩ ♩     ♩   ♩ ♩ ♩         \
       ♩             ♩ ♩               ♩    \n\
       G |   ♩     ♩ ♩     ♩                                       \
       ♩                 ♩                        \n\
       A |     ♩ ♩     ♩     ♩     ♩         ♩           ♩       \
       ♩               ♩ ♩         ♩     ♩          \n\
       B |                                                             ♩       \
       ♩                              \n\
       --------------------------------------------------------------------------------------------------------\n";
  ]

let suite =
  "test suite for A2" >::: List.flatten [ music_tests; music_json_tests ]

let _ = run_test_tt_main suite