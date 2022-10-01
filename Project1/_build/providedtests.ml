open Assert
open Hellocaml

(* These tests are provided by you -- they will NOT be graded *)

(* You should also add additional test cases here to help you   *)
(* debug your program.                                          *)

let provided_tests : suite = [
  Test ("Student-Provided Tests For Problem 1-3", [
    ("case1", assert_eqf (fun () -> 42) prob3_ans );
    ("case2", assert_eqf (fun () -> 42 - 17) (prob3_case2 17) );
    ("case3", assert_eqf (fun () -> prob3_case3) (double (prob3_case2 10)));
  ]);

  GradedTest ("Student-Provided Tests For Problem 4-4", 5, [
    ("interpret1", assert_eqf (fun () -> interpret ctxt2 (Add (Mult (Const 3L, Add (Var "x", Var "y")), Mult (Const 0L, Var "x")))) 27L);
    ("interpret2", (fun () -> try ignore (interpret ctxt2 (Mult (Add (Var "x", Var "z"), Var "y"))); failwith "bad interpret" with Not_found -> ()))
  ]);

  GradedTest ("Student-Provided Tests For Problem 4-5", 8, [
    ("optimize1", assert_eqf (fun () -> optimize (Mult (Add (Const 1L, Const 2L), Mult (Const 3L, Const 7L)))) (Const 63L));
    ("optimize2", assert_eqf (fun () -> optimize (Add (Neg (Const 0L), 
                                                       Add (
                                                            Mult (
                                                                  Add (
                                                                        Mult (Const 0L, Var "x"), 
                                                                        Const 5L), 
                                                                  Const 1L),
                                                            Mult (Const 5L,
                                                                  Neg (Neg (Const 0L)))
                                                       )))) 
                                          (Const 5L));
   ("optimize3", assert_eqf (fun () -> optimize (Add (Neg (Const 0L), 
                                                       Add (
                                                            Mult (
                                                                  Add (
                                                                        Mult (Const 0L, Var "x"), 
                                                                        Const 5L), 
                                                                  Const 1L),
                                                            Mult (Var "y",
                                                                  Neg (Neg (Const 0L)))
                                                       )))) 
                                          (Const 5L));                                       
   ("optimize4", assert_eqf (fun () -> optimize (Add (Neg (Const 0L), 
                                                       Add (
                                                            Mult (
                                                                  Add (
                                                                        Mult (Const 0L, Var "x"), 
                                                                        Const 5L), 
                                                                  Const 1L),
                                                            Mult (Const 5L,
                                                                  Neg (Neg (Var "z")))
                                                       )))) 
                                          (Add ((Const 5L), (Mult (Const 5L, Var "z")))));
   ("optimize5", assert_eqf (fun () -> optimize (Mult (Const 5L, Neg (Neg (Var "z"))))) (Mult (Const 5L, Var "z")));
   ("optimize6", assert_eqf (fun () -> optimize (Neg (Neg (Var "z")))) (Var "z"));
   ("optimize7", assert_eqf (fun () -> optimize (Mult (Neg (Var "x"), Neg (Var "y")))) (Mult (Var "x", Var "y")));
   ("optimize8", assert_eqf (fun () -> optimize (Mult (Neg (Neg (Const 1L)), Mult (Neg (Var "x"), Neg (Var "y"))))) (Mult (Var "x", Var "y")))
  ]);
] 