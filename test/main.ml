open OUnit2
open Music

let piano = [ "C"; "Cs"; "D"; "Ds"; "E"; "F"; "Fs"; "G"; "Gs"; "A"; "As"; "B" ]
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

(** [create_notes_test name input_piano input_scale] constructs an OUnit test
    named [name] that asserts the quality of [expected_output] with
    [create_notes input_piano input_scale]. *)
let create_notes_test (name : string) (input_piano : piano)
    (input_scale : scale) (expected_output : string list) : test =
  name >:: fun _ ->
  assert_equal expected_output
    (create_notes input_piano input_scale)
    ~printer:(pp_list pp_string)

(** [create_melody_test name input_notes input_seed] constructs an OUnit test
    named [name] that asserts the quality of [expected_output] with
    [create_melody input_notes input_seed]. *)
let create_melody_test (name : string) (input_notes : notes) (input_seed : seed)
    (expected_output : string list) : test =
  name >:: fun _ ->
  assert_equal expected_output
    (create_melody input_notes input_seed)
    ~printer:(pp_list pp_string)

let music_tests =
  [
    create_notes_test "c major" piano
      { key = "C"; tonality = major }
      [ "C"; "D"; "E"; "F"; "G"; "A"; "B" ];
    create_notes_test "g major" piano
      { key = "G"; tonality = major }
      [ "C"; "D"; "E"; "Fs"; "G"; "A"; "B" ];
    create_notes_test "d major" piano
      { key = "D"; tonality = major }
      [ "Cs"; "D"; "E"; "Fs"; "G"; "A"; "B" ];
    create_notes_test "a major" piano
      { key = "A"; tonality = major }
      [ "Cs"; "D"; "E"; "Fs"; "Gs"; "A"; "B" ];
    create_notes_test "e major" piano
      { key = "E"; tonality = major }
      [ "Cs"; "Ds"; "E"; "Fs"; "Gs"; "A"; "B" ];
    create_notes_test "b major" piano
      { key = "B"; tonality = major }
      [ "Cs"; "Ds"; "E"; "Fs"; "Gs"; "As"; "B" ];
    create_notes_test "f major" piano
      { key = "F"; tonality = major }
      [ "C"; "D"; "E"; "F"; "G"; "A"; "As" ];
    create_notes_test "a minor" piano
      { key = "A"; tonality = minor }
      [ "C"; "D"; "E"; "F"; "G"; "A"; "B" ];
    create_notes_test "e minor" piano
      { key = "E"; tonality = minor }
      [ "C"; "D"; "E"; "Fs"; "G"; "A"; "B" ];
    create_notes_test "b minor" piano
      { key = "B"; tonality = minor }
      [ "Cs"; "D"; "E"; "Fs"; "G"; "A"; "B" ];
    create_notes_test "d minor" piano
      { key = "D"; tonality = minor }
      [ "C"; "D"; "E"; "F"; "G"; "A"; "As" ];
    create_notes_test "g minor" piano
      { key = "G"; tonality = minor }
      [ "C"; "D"; "Ds"; "F"; "G"; "A"; "As" ];
    create_notes_test "c minor" piano
      { key = "C"; tonality = minor }
      [ "C"; "D"; "Ds"; "F"; "G"; "Gs"; "As" ];
    create_notes_test "f minor" piano
      { key = "F"; tonality = minor }
      [ "C"; "Cs"; "Ds"; "F"; "G"; "Gs"; "As" ];
    create_melody_test "basic c major melody"
      [ "C"; "D"; "E"; "F"; "G"; "A"; "B" ]
      [ 0; 1; 2; 3; 4; 5; 6; 0; 1; 2; 3; 4; 5; 6 ]
      [ "C"; "D"; "E"; "F"; "G"; "A"; "B"; "C"; "D"; "E"; "F"; "G"; "A"; "B" ];
    create_melody_test "random c major melody"
      [ "C"; "D"; "E"; "F"; "G"; "A"; "B" ]
      [ 6; 4; 3; 5; 1; 3; 0; 2; 3; 3; 4; 6; 1; 5 ]
      [ "B"; "G"; "F"; "A"; "D"; "F"; "C"; "E"; "F"; "F"; "G"; "B"; "D"; "A" ];
    create_melody_test "basic b minor melody"
      [ "Cs"; "D"; "E"; "Fs"; "G"; "A"; "B" ]
      [ 0; 1; 2; 3; 4; 5; 6; 0; 1; 2; 3; 4; 5; 6 ]
      [
        "Cs"; "D"; "E"; "Fs"; "G"; "A"; "B"; "Cs"; "D"; "E"; "Fs"; "G"; "A"; "B";
      ];
    create_melody_test "random b minor melody"
      [ "Cs"; "D"; "E"; "Fs"; "G"; "A"; "B" ]
      [ 3; 4; 6; 5; 1; 2; 0; 0; 1; 3; 5; 6; 1; 3 ]
      [
        "Fs";
        "G";
        "B";
        "A";
        "D";
        "E";
        "Cs";
        "Cs";
        "D";
        "Fs";
        "A";
        "B";
        "D";
        "Fs";
      ];
  ]

let suite = "test suite for A2" >::: List.flatten [ music_tests ]
let _ = run_test_tt_main suite