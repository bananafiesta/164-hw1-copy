open OUnit2
open S_exp
open Lib.Arith

(******************************************************************************)
(* Tasks *)

(* Task 2.1 *)

let test_is_bin : test_ctxt -> unit =
  fun _ ->
    (* failwith "TODO" *)
    List.iter
      ( fun (expected, input) ->
          assert_equal
            ~printer:string_of_bool
            expected
            (is_bin input)
      )
      [
        (true, Lst [Sym "+"; Num 5; Num 3]);
        (true, Lst [Sym "*"; Lst [Sym "+"; Num 13; Num 9]; Num 18]);
        (false, Sym "*");
        (false, Lst [Sym "+"; Num 1; Num 2; Num 3]);
        (false, Lst [Sym "*"; Lst [Sym "+"; Num 1; Num 2]]);
        (true, Num 3)
      ]

(* Task 2.3 *)

let test_interp_bin : test_ctxt -> unit =
  fun _ ->
    (* failwith "TODO" *)
    List.iter
      ( fun (expected, input) ->
          assert_equal
            ~printer:string_of_int
            expected
            (interp_bin input)
      )
      [
        (32, Lst [Sym "+"; Num 20; Num 12]);
        (9, Num 9);
        (63, Lst [Sym "*"; Lst [Sym "*"; Num 3; Num 3]; Lst [Sym "+"; Num 4; Num 3]])
      ];
    List.iter
    ( fun (expected, input) ->
        assert_raises
          (Stuck expected)
          (fun () -> (interp_bin input))
    )
    [
      (Lst [Sym "*"], Lst [Sym "*"]);
      (Lst [Sym "+"; Num 8], Lst [Sym "*"; Lst [Sym "+"; Num 8]; Num 3])
    ]
    (* assert_equal ~printer:string_of_int 32 (interp_bin (Lst[Sym "+"; Num 20; Num 12]))
    assert_equal ~printer:string_of_int 9 (interp_bin 9) *)
    

(* Task 2.5 *)

let test_interp_instr : test_ctxt -> unit =
  fun _ ->
    (* failwith "TODO" *)
    List.iter
      ( fun (expected, input1, input2) ->
          assert_equal
            expected
            (interp_instr input1 input2)
      )
      [
        ([12; 2; 1], [4; 3; 2; 1], Mul);
        ([9; 8; 7], [8; 7], Push 9);
        ([37], [], Push 37);
        ([89], [88; 1], Add)
      ];
    List.iter
    ( fun (expected, input1, input2) ->
        assert_raises
          (ShortStack expected)
          (fun () -> (interp_instr input1 input2))
    )
    [
      ([96], [96], Add);
      ([], [], Mul);
    ]

let test_interp_program : test_ctxt -> unit =
  fun _ ->
    (* failwith "TODO" *)
    List.iter
      ( fun (expected, input) ->
          assert_equal
            expected
            (interp_program input)
      )
      [
        (30, [Push 16; Push 14; Add]);
        (49, [Push 1; Push 2; Push 3; Push 7; Push 7; Mul]);
        (15, [Push 5; Push 3; Mul]);
        (8, [Push 1; Push 99; Push 35; Push 2; Push 6; Add])
      ];
    List.iter
    ( fun (expected, input) ->
        assert_raises
          (ShortStack expected)
          (fun () -> (interp_program input))
    )
    [
      ([], [Mul]);
      ([], [Add]);
      ([88], [Push 88; Add]);
      ([298], [Push 298; Mul])
    ]

(* Task 2.7 *)

let test_compile_bin : test_ctxt -> unit =
  fun _ ->
    (* failwith "TODO" *)
    List.iter
      ( fun (expected, input) ->
          assert_equal
            expected
            (compile_bin input)
      )
      [
        ([Push 1; Push 2; Add], Lst[Sym "+"; Num 1; Num 2]);
        ([Push 3; Push 2; Add; Push 22; Push 3; Mul; Mul], Lst[Sym "*"; Lst[Sym "+"; Num 3; Num 2]; Lst[Sym "*"; Num 22; Num 3]]);
        ([Push 66], Num 66)

      ];
    List.iter
    ( fun (expected, input) ->
        assert_raises
          (Stuck expected)
          (fun () -> (compile_bin input))
    )
    [
      (Lst[Sym "*"], Lst[Sym "*"]);
      (Lst[Sym "+"; Num 3], Lst[Sym "*"; Lst[Sym "+"; Num 1; Num 2]; Lst[Sym "+"; Num 3]])
    ]

(* Task 2.9 *)

let test_compile_versus_interp_bin : test_ctxt -> unit =
  fun _ ->
    (* failwith "TODO" *)
    List.iter
      ( fun (input) ->
          assert_equal
            (interp_bin input)
            (interp_program(compile_bin input))
      )
      [
        (Lst[Sym "*"; Lst[Sym "+"; Num 3; Num 2]; Lst[Sym "*"; Num 22; Num 3]]);
        (Num 8862);
        (Lst[Sym "+"; Num 2713; Num 28371]);
        (Lst[Sym "+"; Lst[Sym "*"; Num 2; Lst[Sym "+"; Num 6; Num 10]]; Lst[Sym "+"; Num 13; Num 23]])

      ]

(* Task 3.3 *)

let test_variadic : test_ctxt -> unit =
  fun _ ->
    (* failwith "TODO" *)
    List.iter
      ( fun (input) ->
          assert_equal
            (interp_bin (desugar_variadic input))
            (interp_variadic input)
      )
      [
        (Lst[Sym "*"; Lst[Sym "+"; Num 3; Num 2]; Lst[Sym "*"; Num 22; Num 3]]);
        (Num 8862);
        (Lst[Sym "+"; Num 2713; Num 28371]);
        (Lst[Sym "+"; Lst[Sym "*"; Num 2; Lst[Sym "+"; Num 6; Num 10]]; Lst[Sym "+"; Num 13; Num 23]]);
        (Lst[Sym "+"]);
        (Lst[Sym "*"]);
        (Lst[Sym "+"; Num 0; Num 1; Num 2; Num 3]);
        (Lst[Sym "*"; Num 2; Num 3; Num 4]);
        (Lst[Sym "+"; Num 4; Lst[Sym "*"; Num 3; Num 4; Lst[Sym "+"; Num 5; Num 3; Num 4]]; Lst[Sym "+"; Num 2; Num 5; Num 9; Lst[Sym "*"; Num 3; Num 9]]])

      ];
      List.iter
      ( fun (expected, input) ->
          assert_equal
            (expected)
            (interp_variadic input)
      )
      [
        (6, Lst[Sym "+"; Num 0; Num 1; Num 2; Num 3]);
        (24, Lst[Sym "*"; Num 2; Num 3; Num 4]);
        (191, (Lst[Sym "+"; Num 4; Lst[Sym "*"; Num 3; Num 4; Lst[Sym "+"; Num 5; Num 3; Num 4]]; Lst[Sym "+"; Num 2; Num 5; Num 9; Lst[Sym "*"; Num 3; Num 9]]]))


      ]

(******************************************************************************)
(* Test runner *)

let _ =
  run_test_tt_main
    ( "arith tests" >:::
        [ "is_bin" >:: test_is_bin
        ; "interp_bin" >:: test_interp_bin
        ; "interp_instr" >:: test_interp_instr
        ; "interp_program" >:: test_interp_program
        ; "compile_bin" >:: test_compile_bin
        ; "compiling vs. interpreting" >:: test_compile_versus_interp_bin
        ; "variadic" >:: test_variadic
        ]
    )
