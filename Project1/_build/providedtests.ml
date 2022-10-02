open Assert
open Hellocaml

(* These tests are provided by you -- they will NOT be graded *)

(* You should also add additional test cases here to help you   *)
(* debug your program.                                          *)

let e4 : exp = (Add (Mult (Const 3L, Add (Var "x", Var "y")), Mult (Const 0L, Var "x")))
(* note that under ctxt2 = [("x", 2L); ("y", 7L)] this is (3*(x+y))+(0*x) = (3*9)+0 = 0 *)
let e5 = (Mult (Add (Var "x", Var "z"), Var "y"))
(* note that under ctxt2, this should yield a bad interpret exception. (x+z)*y. Under ctxt3 = 21 *)

let e6 = (Mult (Add (Const 1L, Const 2L), Mult (Const 3L, Const 7L)))
(* note that this evaluates to 63L = (1+2)*(3*7) *)

let e7 = (Add (Neg (Const 0L), 
               Add (
                    Mult (
                          Add (
                                Mult (Const 0L, Var "x"), 
                                Const 5L), 
                          Const 1L),
                    Mult (Const 5L,
                          Neg (Neg (Const 0L)))
               )))
(* note that this evaluates to 5L *)                                                       

let e8 = (Add (Neg (Const 0L), 
               Add (
                    Mult (
                          Add (
                                Mult (Const 0L, Var "x"), 
                                Const 5L), 
                          Const 1L),
                    Mult (Var "y",
                          Neg (Neg (Const 0L)))
               )))
(* note that this evaluates to 5L *)

let ctxt3 = [("x", 2L); ("y", 3L); ("z", 5L)]

let e9 = (Add (Neg (Const 0L), (* 30L *)
               Add (    (* 30L *)
                    Mult ( (* 5L *)
                          Add (     (* 5L *)
                                Mult (Const 0L, Var "x"), (* 0L *)
                                Const 5L), 
                          Const 1L),
                    Mult (Const 5L,       (* 25L *)
                          Neg (Neg (Var "z")))  (* 5L *)
               )))
(* note that under ctxt3, this evaluates to 30L *)

let e10 = (Mult (Const 5L, Neg (Neg (Var "z"))))
(* under ctxt3, this evaluates to 25L *)

let e11 = (Neg (Neg (Var "z")))
(* under ctxt3, this evaluates to 5L *)

let e12 = (Mult (Neg (Var "x"), Neg (Var "y")))
(* under ctxt3, this evaluates to 6L *)

let e13 = (Mult (Neg (Neg (Const 1L)), Mult (Neg (Var "x"), Neg (Var "y"))))
(* under ctxt3, this evaluates to 6L *)

let e14 = (Add (Mult (Const 3L, Add (Var "x", Var "y")), Mult (Const 0L, Var "x")))

let provided_tests : suite = [
  Test ("Student-Provided Tests For Problem 1-3", [
    ("case1", assert_eqf (fun () -> 42) prob3_ans );
    ("case2", assert_eqf (fun () -> 42 - 17) (prob3_case2 17) );
    ("case3", assert_eqf (fun () -> prob3_case3) (double (prob3_case2 10)));
  ]);

  Test ("Student-Provided Tests For Problem 4-4", [
    ("interpret1", assert_eqf (fun () -> interpret ctxt2 e4) 27L);
    ("interpret2", (fun () -> try ignore (interpret ctxt2 e5); failwith "bad interpret" with Not_found -> ()))
  ]);

  Test ("Student-Provided Tests For Problem 4-5", [
    ("optimize1", assert_eqf (fun () -> optimize e6) (Const 63L));
    ("optimize2", assert_eqf (fun () -> optimize e7) (Const 5L));
    ("optimize3", assert_eqf (fun () -> optimize e8) (Const 5L));                                       
    ("optimize4", assert_eqf (fun () -> optimize e9) (Add ((Const 5L), (Mult (Const 5L, Var "z")))));
    ("optimize5", assert_eqf (fun () -> optimize e10) (Mult (Const 5L, Var "z")));
    ("optimize6", assert_eqf (fun () -> optimize e11) (Var "z"));
    ("optimize7", assert_eqf (fun () -> optimize e12) (Mult (Var "x", Var "y")));
    ("optimize8", assert_eqf (fun () -> optimize e13) (Mult (Var "x", Var "y")))
  ]);
  
  Test ("Student-Provided Tests For Problem 5", [
    ("case1", assert_eqf (fun () -> compile e1) ([IPushC 2L; IPushC 3L; IMul]));
    ("case2", assert_eqf (fun () -> interpret ctxt2 e1) (run ctxt2 (compile e1)));
    ("case3", assert_eqf (fun () -> interpret ctxt2 e14) (run ctxt2 (compile e14)));
    ("case4", assert_eqf (fun () -> interpret ctxt3 e6) (run ctxt3 (compile e6)));
    ("case5", assert_eqf (fun () -> interpret ctxt3 e7) (run ctxt3 (compile e7)));
    ("case6", assert_eqf (fun () -> interpret ctxt3 e8) (run ctxt3 (compile e8)));
    ("case7", assert_eqf (fun () -> interpret ctxt3 e9) (run ctxt3 (compile e9)));
    ("case8", assert_eqf (fun () -> interpret ctxt3 e10) (run ctxt3 (compile e10)));
    ("case9", assert_eqf (fun () -> interpret ctxt3 e11) (run ctxt3 (compile e11)));
    ("case10", assert_eqf (fun () -> interpret ctxt3 e12) (run ctxt3 (compile e12)));
    ("case11", assert_eqf (fun () -> interpret ctxt3 e13) (run ctxt3 (compile e13)));
  ]);
] 