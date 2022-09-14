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
    failwith "TODO"

let test_interp_program : test_ctxt -> unit =
  fun _ ->
    failwith "TODO"

(* Task 2.7 *)

let test_compile_bin : test_ctxt -> unit =
  fun _ ->
    failwith "TODO"

(* Task 2.9 *)

let test_compile_versus_interp_bin : test_ctxt -> unit =
  fun _ ->
    failwith "TODO"

(* Task 3.3 *)

let test_variadic : test_ctxt -> unit =
  fun _ ->
    failwith "TODO"

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
