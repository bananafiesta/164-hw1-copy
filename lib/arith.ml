open S_exp

(******************************************************************************)
(* Task 1 *)

(* Task 1.1 *)

let rec string_of_s_exp : s_exp -> string =
  fun exp ->
    (* failwith "TODO" *)
    match exp with
    | Num n -> string_of_int n
    | Sym x -> x
    | Lst [] -> "()"
    | Lst j -> "(" ^ String.concat " " (List.map string_of_s_exp j) ^ ")"
  


(******************************************************************************)
(* Task 2 *)

(* Task 2.1 is found in `test/test_arith.ml` *)

(* Task 2.2 *)

let rec is_bin : s_exp -> bool =
  fun exp ->
    (* failwith "TODO" *)
    match exp with
    | Num _ -> true
    | Lst [Sym "*" | Sym "+"; arg2; arg3] -> is_bin arg2 && is_bin arg3
    | _ -> false

(* Task 2.3 is found in `test/test_arith.ml` *)

(* Task 2.4 *)

exception Stuck of s_exp

let rec interp_bin : s_exp -> int =
  fun exp ->
    (* failwith "TODO" *)
    match exp with
    | Num n -> n
    | Lst [Sym "*"; arg2; arg3] -> (interp_bin arg2) * (interp_bin arg3)
    | Lst [Sym "+"; arg2; arg3] -> (interp_bin arg2) + (interp_bin arg3)
    | _ -> raise (Stuck exp)


(* Task 2.5 is found in `test/test_arith.ml` *)

(* Task 2.6 *)

type instr
  = Push of int
  | Add
  | Mul

type stack =
  int list

exception ShortStack of stack

let interp_instr : stack -> instr -> stack =
  fun stack instr ->
    (* failwith "TODO" *)
    match instr with
    | Add -> 
      (if ((List.length stack) >= 2) then 
        (let arg1 = (List.hd stack) and arg2 = (List.hd (List.tl stack)) in
          (List.append ([arg1 + arg2]) (List.tl (List.tl stack))))
      else raise (ShortStack stack))
    | Mul -> 
      (if ((List.length stack) >= 2) then 
        (let arg1 = (List.hd stack) and arg2 = (List.hd (List.tl stack)) in
          (List.append ([arg1 * arg2]) (List.tl (List.tl stack))))
      else raise (ShortStack stack))
    | Push n -> List.append [n] stack

let interp_program : instr list -> int =
  fun instrs ->
    (* failwith "TODO" *)
    match instrs with
    | [] -> 0
    | _ ->
      let stack = [] in 
        List.hd(List.fold_left interp_instr stack instrs)

(* Task 2.7 is found in `test/test_arith.ml` *)

(* Task 2.8 *)

let rec compile_bin : s_exp -> instr list =
  fun exp ->
    (* failwith "TODO" *)
    match exp with 
    | Num n -> [Push n]
    | Lst [Sym "+"; arg1; arg2] -> (compile_bin arg1) @ (compile_bin arg2) @ [Add]
    | Lst [Sym "*"; arg1; arg2] -> (compile_bin arg1) @ (compile_bin arg2) @ [Mul]
    | _ -> raise (Stuck exp)

(* Task 2.9 is found in `test/test_arith.ml` *)

(******************************************************************************)
(* Task 3 *)

(* Task 3.1 *)

let rec desugar_variadic : s_exp -> s_exp =
  fun exp ->
    (* failwith "TODO" *)
    match exp with
    | Num n -> Num n;
    | Lst [Sym "+"] -> Num 0
    | Lst [Sym "*"] -> Num 1
    (* | Lst [Sym s; arg1; arg2] -> (Lst [Sym s; desugar_variadic arg1; desugar_variadic arg2]) *)
    | Lst [Sym _; arg1] -> desugar_variadic arg1
    | Lst(Sym "+" :: arg :: rest) -> Lst [Sym "+"; desugar_variadic arg; desugar_variadic (Lst(Sym "+" :: rest))]
    | Lst(Sym "*" :: arg :: rest) -> Lst [Sym "*"; desugar_variadic arg; desugar_variadic (Lst(Sym "*" :: rest))]
    | _ -> Lst []


(* Task 3.2 *)

let rec interp_variadic : s_exp -> int =
  fun exp ->
    (* failwith "TODO" *)
    match exp with
    | Num n -> n
    | Lst [Sym "+"] -> 0
    | Lst [Sym "*"] -> 1
    | Lst [Sym _; arg1] -> interp_variadic arg1
    | Lst(Sym "+" :: arg :: rest) -> (interp_variadic arg) + (interp_variadic (Lst(Sym "+" :: rest)))
    | Lst(Sym "*" :: arg :: rest) -> (interp_variadic arg) * (interp_variadic (Lst(Sym "*" :: rest)))
    | _ -> raise (Stuck exp)

(* Task 3.3 is found in `test/test_arith.ml` *)
