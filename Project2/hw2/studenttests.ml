open Assert
open X86
open Simulator
open Asm
(* You can use this file for additional test cases to help your *)
(* implementation.                                              *)
let cc_test (s:string) (n: int) (m: mach) (fo', fs', fz') (f: mach -> bool) () =
  let m' = {m with flags = {fo=fo';fs=fs';fz=fz'}} in
  for i=1 to n do step m' done;
  if (f m') then () else failwith s

let cs_test (n:int) (m:mach) (fo',fs',fz') =
  cc_test (Printf.sprintf "expected OF:%b SF:%b ZF:%b but got 0F:%b SF:%b ZF:%b" fo' fs' fz' m.flags.fo m.flags.fs m.flags.fz)
    n m (not fo',not fs',not fz')
    (fun m -> m.flags.fo = fo' && m.flags.fs = fs' && m.flags.fz = fz')

let test_machine (bs: sbyte list): mach =
  let mem = (Array.make mem_size (Byte '\x00')) in
  Array.blit (Array.of_list bs) 0 mem 0 (List.length bs);
  let regs = Array.make nregs 0L in
  regs.(rind Rip) <- mem_bot;
  regs.(rind Rsp) <- Int64.sub mem_top 8L;
  { flags = {fo = false; fs = false; fz = false};
    regs = regs;
    mem = mem
  }

let cc_neg_2 = test_machine
  [InsB0 (Movq, [~$1; ~%Rax]);InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag
  ;InsB0 (Negq, [~%Rax]);InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag;InsFrag
  ]

let condition_flag_set_tests =
  [ 
   ("cc_neg_2", cs_test 2 cc_neg_2 (false, true, false))
  ]

let provided_tests : suite = [
  (*Test("XXXXXXXXXX", condition_flag_set_tests);*)
]