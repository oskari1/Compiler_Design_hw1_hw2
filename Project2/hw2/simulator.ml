(* X86lite Simulator *)

(* See the documentation in the X86lite specification, available on the 
   course web pages, for a detailed explanation of the instruction
   semantics.
*)

open X86

(* simulator machine state -------------------------------------------------- *)

let mem_bot = 0x400000L          (* lowest valid address *)
let mem_top = 0x410000L          (* one past the last byte in memory *)
let mem_size = Int64.to_int (Int64.sub mem_top mem_bot)
let nregs = 17                   (* including Rip *)
let ins_size = 8L                (* assume we have a 8-byte encoding *)
let exit_addr = 0xfdeadL         (* halt when m.regs(%rip) = exit_addr *)

(* Your simulator should raise this exception if it tries to read from or
   store to an address not within the valid address space. *)
exception X86lite_segfault

(* The simulator memory maps addresses to symbolic bytes.  Symbolic
   bytes are either actual data indicated by the Byte constructor or
   'symbolic instructions' that take up eight bytes for the purposes of
   layout.

   The symbolic bytes abstract away from the details of how
   instructions are represented in memory.  Each instruction takes
   exactly eight consecutive bytes, where the first byte InsB0 stores
   the actual instruction, and the next seven bytes are InsFrag
   elements, which aren't valid data.

   For example, the two-instruction sequence:
        at&t syntax             ocaml syntax
      movq %rdi, (%rsp)       Movq,  [~%Rdi; Ind2 Rsp]
      decq %rdi               Decq,  [~%Rdi]

   is represented by the following elements of the mem array (starting
   at address 0x400000):

       0x400000 :  InsB0 (Movq,  [~%Rdi; Ind2 Rsp])
       0x400001 :  InsFrag
       0x400002 :  InsFrag
       0x400003 :  InsFrag
       0x400004 :  InsFrag
       0x400005 :  InsFrag
       0x400006 :  InsFrag
       0x400007 :  InsFrag
       0x400008 :  InsB0 (Decq,  [~%Rdi])
       0x40000A :  InsFrag
       0x40000B :  InsFrag
       0x40000C :  InsFrag
       0x40000D :  InsFrag
       0x40000E :  InsFrag
       0x40000F :  InsFrag
       0x400010 :  InsFrag
*)
type sbyte = InsB0 of ins       (* 1st byte of an instruction *)
           | InsFrag            (* 2nd - 8th bytes of an instruction *)
           | Byte of char       (* non-instruction byte *)

(* memory maps addresses to symbolic bytes *)
type mem = sbyte array

(* Flags for condition codes *)
type flags = { mutable fo : bool
             ; mutable fs : bool
             ; mutable fz : bool
             }

(* Register files *)
type regs = int64 array

(* Complete machine state *)
type mach = { flags : flags
            ; regs : regs
            ; mem : mem
            }

(* simulator helper functions ----------------------------------------------- *)

(* The index of a register in the regs array *)
let rind : reg -> int = function
  | Rip -> 16
  | Rax -> 0  | Rbx -> 1  | Rcx -> 2  | Rdx -> 3
  | Rsi -> 4  | Rdi -> 5  | Rbp -> 6  | Rsp -> 7
  | R08 -> 8  | R09 -> 9  | R10 -> 10 | R11 -> 11
  | R12 -> 12 | R13 -> 13 | R14 -> 14 | R15 -> 15

(* Helper functions for reading/writing sbytes *)

(* Convert an int64 to its sbyte representation *)
let sbytes_of_int64 (i:int64) : sbyte list =
  let open Char in 
  let open Int64 in
  List.map (fun n -> Byte (shift_right i n |> logand 0xffL |> to_int |> chr))
           [0; 8; 16; 24; 32; 40; 48; 56]

(* Convert an sbyte representation to an int64 *)
let int64_of_sbytes (bs:sbyte list) : int64 =
  let open Char in
  let open Int64 in
  let f b i = match b with
    | Byte c -> logor (shift_left i 8) (c |> code |> of_int)
    | _ -> 0L
  in
  List.fold_right f bs 0L

(* Convert a string to its sbyte representation *)
let sbytes_of_string (s:string) : sbyte list =
  let rec loop acc = function
    | i when i < 0 -> acc
    | i -> loop (Byte s.[i]::acc) (pred i)
  in
  loop [Byte '\x00'] @@ String.length s - 1

(* Serialize an instruction to sbytes *)
let sbytes_of_ins (op, args:ins) : sbyte list =
  let check = function
    | Imm (Lbl _) | Ind1 (Lbl _) | Ind3 (Lbl _, _) -> 
      invalid_arg "sbytes_of_ins: tried to serialize a label!"
    | o -> ()
  in
  List.iter check args;
  [InsB0 (op, args); InsFrag; InsFrag; InsFrag;
   InsFrag; InsFrag; InsFrag; InsFrag]

(* Serialize a data element to sbytes *)
let sbytes_of_data : data -> sbyte list = function
  | Quad (Lit i) -> sbytes_of_int64 i
  | Asciz s -> sbytes_of_string s
  | Quad (Lbl _) -> invalid_arg "sbytes_of_data: tried to serialize a label!"


(* It might be useful to toggle printing of intermediate states of your 
   simulator. Our implementation uses this mutable flag to turn on/off
   printing.  For instance, you might write something like:

     [if !debug_simulator then print_endline @@ string_of_ins u; ...]

*)
let debug_simulator = ref false

(* Interpret a condition code with respect to the given flags. *)
let interp_cnd {fo; fs; fz} : cnd -> bool = fun c ->
  match c with
  | Eq -> fz 
  | Neq -> not fz 
  | Lt -> (fs && (not fo)) || (not fs && fo)
  | Le -> (fs <> fo) || fz
  | Gt -> not ((fs <> fo) || fz)  
  | Ge -> fs = fo 

(* Maps an X86lite address into Some OCaml array index,
   or None if the address is not within the legal address space. *)
let map_addr (addr:quad) : int option = 
  if addr > mem_top || addr < mem_bot then None else Some (Int64.to_int (Int64.sub addr 0x400000L)) 

(* Given memory : sbyte array and an address in hex, translate the address into int
   and use it to access 8 bytes starting at the desired address *)
let fetch (m:mem) (addr:quad) : (sbyte list) = 
  let addr' = map_addr addr in
  match addr' with 
  | Some add -> List.map (fun i -> m.(add + i)) [0;1;2;3;4;5;6;7]   (* note that since we have 64-bit machine, we fetch 8 bytes *)
  | None -> raise X86lite_segfault

let write_quad_to_mem (m:mem) (addr:quad) (value:quad) : unit =
  let sbyte_values = sbytes_of_int64 value in
  for n = 0 to 7 do 
    m.(Int64.to_int addr + n) <-  List.nth sbyte_values n
  done

let rec read (m:mach) (operand:operand) : quad = 
  match operand with
  | Imm (Lit lit) -> lit
  | Reg reg -> m.regs.(rind reg)
  | Ind1 (Lit lit) -> let fetched_sbytes = fetch (m.mem) lit in int64_of_sbytes fetched_sbytes
  | Ind2 reg -> let fetched_sbytes = fetch (m.mem) (read m (Reg reg)) in int64_of_sbytes fetched_sbytes
  | Ind3 (Lit lit, reg) ->  let fetched_sbytes = fetch (m.mem) (Int64.add lit (read m (Reg reg))) in int64_of_sbytes fetched_sbytes
  | _ -> raise X86lite_segfault

let write (m:mach) (operand:operand) (value:quad) :unit =
  match operand with 
  | Reg reg -> m.regs.(rind reg) <- value
  | Ind1 (Lit lit) -> write_quad_to_mem (m.mem) lit value 
  | Ind2 reg -> write_quad_to_mem (m.mem) (read m (Reg reg)) value 
  | Ind3 (Lit lit, reg) -> write_quad_to_mem (m.mem) (Int64.add lit (read m (Reg reg))) value 
  | _ -> raise X86lite_segfault

let get_op (ins:sbyte) : opcode =
    match ins with
    | InsB0 (op', _) -> op'
    | _ -> raise X86lite_segfault

let get_operands (ins:sbyte) : (operand list) =
  match ins with
  | InsB0 (_, operands) -> operands 
  | _ -> raise X86lite_segfault

let update_state (op:opcode) (operands : operand list) (m:mach) = 
  if List.mem op [Negq; Incq; Decq; Notq] then 
    begin
      let dst = List.hd operands in
      let src = read m dst in 
      match op with
      | Negq -> write m dst (Int64.neg src) 
      | Incq -> write m dst (Int64.succ src)
      | Decq -> write m dst (Int64.pred src)
      | Notq -> write m dst (Int64.lognot src)
      | _ -> raise X86lite_segfault
    end
  else 
    if List.mem op [Addq; Subq; Imulq; Andq; Orq; Xorq; Sarq; Shlq; Shrq] then
      begin
        let src = List.hd operands in
        let dst = List.hd (List.rev operands) in
        match op with
        | Addq -> write m dst (Int64.add (read m src) (read m dst))  
        | Subq -> write m dst (Int64.sub (read m src) (read m dst))  
        | Imulq -> write m dst (Int64.mul (read m src) (read m dst))  
        | Andq -> write m dst (Int64.logand (read m src) (read m dst))  
        | Orq -> write m dst (Int64.logor (read m src) (read m dst))  
        | Xorq -> write m dst (Int64.logxor (read m src) (read m dst))  
        | Sarq -> write m dst (Int64.shift_right (read m dst) (Int64.to_int (read m src)))  
        | Shlq -> write m dst (Int64.shift_left (read m dst) (Int64.to_int (read m src)))  
        | Shrq -> write m dst (Int64.shift_right_logical (read m dst) (Int64.to_int (read m src)))  
        | _ -> raise X86lite_segfault
      end
    else
      match op with
      | Set cc -> if interp_cnd (m.flags) cc then failwith "unimplemented"
      | _ -> raise X86lite_segfault

(* Simulates one step of the machine:
    - fetch the instruction at %rip
    - compute the source and/or destination information from the operands
    - simulate the instruction semantics
    - update the registers and/or memory appropriately
    - set the condition flags
*)
let step (m:mach) : unit =
  let ins = List.hd (fetch (m.mem) (read m (Reg Rip))) in
  (* extract the operation and the operands *)
  let op = get_op ins in
  let operands = get_operands ins in 
  update_state op operands m

(* Runs the machine until the rip register reaches a designated
   memory address. Returns the contents of %rax when the 
   machine halts. *)
let run (m:mach) : int64 = 
  while m.regs.(rind Rip) <> exit_addr do step m done;
  m.regs.(rind Rax)

(* assembling and linking --------------------------------------------------- *)

(* A representation of the executable *)
type exec = { entry    : quad              (* address of the entry point *)
            ; text_pos : quad              (* starting address of the code *)
            ; data_pos : quad              (* starting address of the data *)
            ; text_seg : sbyte list        (* contents of the text segment *)
            ; data_seg : sbyte list        (* contents of the data segment *)
            }

(* Assemble should raise this when a label is used but not defined *)
exception Undefined_sym of lbl

(* Assemble should raise this when a label is defined more than once *)
exception Redefined_sym of lbl

(* Convert an X86 program into an object file:
   - separate the text and data segments
   - compute the size of each segment
      Note: the size of an Asciz string section is (1 + the string length)
            due to the null terminator

   - resolve the labels to concrete addresses and 'patch' the instructions to 
     replace Lbl values with the corresponding Imm values.

   - the text segment starts at the lowest address
   - the data segment starts after the text segment

  HINT: List.fold_left and List.fold_right are your friends.
 *)
let assemble (p:prog) : exec =
failwith "assemble unimplemented"

(* Convert an object file into an executable machine state. 
    - allocate the mem array
    - set up the memory state by writing the symbolic bytes to the 
      appropriate locations 
    - create the inital register state
      - initialize rip to the entry point address
      - initializes rsp to the last word in memory 
      - the other registers are initialized to 0
    - the condition code flags start as 'false'

  Hint: The Array.make, Array.blit, and Array.of_list library functions 
  may be of use.
*)
let load {entry; text_pos; data_pos; text_seg; data_seg} : mach = 
failwith "load unimplemented"
