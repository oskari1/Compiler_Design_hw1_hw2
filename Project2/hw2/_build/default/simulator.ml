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
   and use it to access 8 bytes starting at the desired address. Note comment earlier
   "Each instruciton takes up exactly 8 bytes but only the first byte contains information about the instruction"
   That's why we use map to fetch the 8 bytes that follow right after addr. *)
let read_quad_from_mem (m:mem) (addr:quad) : (sbyte list) = 
  let addr' = map_addr addr in
  match addr' with 
  | Some add -> List.map (fun i -> m.(add + i)) [0;1;2;3;4;5;6;7]   
  | None -> raise X86lite_segfault

(* Given memory : sbyte array and an address in hex, translate the address into int
   and use it to write to the 8 bytes starting at the desired address the sbyte representation of the input data value *)
let write_quad_to_mem (m:mem) (addr:quad) (value:quad) : unit =
  let sbyte_values = sbytes_of_int64 value in
  let addr' = map_addr addr in 
  match addr' with
  | Some add -> 
    begin 
      for n = 0 to 7 do 
        m.(add + n) <-  List.nth sbyte_values n
      done
    end
  | None -> raise X86lite_segfault

let rec read (m:mach) (operand:operand) : quad = 
  match operand with
  | Imm (Lit lit) -> lit
  | Reg reg -> m.regs.(rind reg)
  | Ind1 (Lit lit) -> let fetched_sbytes = read_quad_from_mem (m.mem) lit in int64_of_sbytes fetched_sbytes
  | Ind2 reg -> let fetched_sbytes = read_quad_from_mem (m.mem) (read m (Reg reg)) in int64_of_sbytes fetched_sbytes
  | Ind3 (Lit lit, reg) ->  let fetched_sbytes = read_quad_from_mem (m.mem) (Int64.add lit (read m (Reg reg))) in int64_of_sbytes fetched_sbytes
  | _ -> raise X86lite_segfault

let compute_Ind_addr (operand:operand) (m:mach) : quad =
  match operand with
  | Ind1 (Lit lit) -> lit
  | Ind2 reg -> read m (Reg reg) 
  | Ind3 (Lit lit, reg) -> Int64.add lit (read m (Reg reg))
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

let rec update_state_non_ALU (op:opcode) (operands : operand list) (m:mach) :unit = 
  match op with
  | Set cc -> 
    let dst = List.hd operands in   
    let value = read m dst in
    let zeroed_lower_bytes = Int64.logand value 0xffL in
    begin
      if interp_cnd (m.flags) cc then write m dst (Int64.add zeroed_lower_bytes 1L) 
      else write m dst zeroed_lower_bytes 
    end
  | Leaq -> 
    let addr = compute_Ind_addr (List.hd operands) m in
    let dst = List.hd (List.rev operands) in
    write m dst addr 
  | Movq -> 
    let src = read m (List.hd operands) in
    let dst = List.hd (List.rev operands) in
    write m dst src
  | Pushq -> 
    let src = read m (List.hd operands) in 
    let rsp_val = read m (Reg Rsp) in
    begin
      write m (Reg Rsp) (Int64.sub rsp_val 8L);
      write m (Ind2 Rsp) src
    end
  | Popq -> 
    let dst = List.hd operands in 
    let mem_rsp = read m (Ind2 Rsp) in
    let rsp_val = read m (Reg Rsp) in 
    begin
      write m dst mem_rsp;
      write m (Reg Rsp) (Int64.add rsp_val 8L)
    end
  | Jmp -> let src = read m (List.hd operands) in write m (Reg Rip) src
  | Callq -> let src = List.hd operands in update_state_non_ALU Pushq [Reg Rip] m; update_state_non_ALU Jmp [src] m
  | Retq -> update_state_non_ALU Popq [Reg Rip] m
  | J cc -> 
    let src = List.hd operands in
    let rip_val = read m (Reg Rip) in
    begin 
      if interp_cnd (m.flags) cc 
      then update_state_non_ALU Jmp [src] m
      else write m (Reg Rip) (Int64.add rip_val 8L) 
    end
  | _ -> raise X86lite_segfault

let set_cc_unary_ALU (m:mach) (result:quad) (op:opcode) (dst_val:quad) = 
  let set_flags (overflow_op: quad -> Int64_overflow.t) = 
    begin
      let result_t : Int64_overflow.t = overflow_op dst_val in
      m.flags.fo <- result_t.overflow;
      m.flags.fs <- result < 0L;
      m.flags.fz <- result = 0L
    end in
  match op with
  | Negq -> set_flags Int64_overflow.neg 
  | Decq -> set_flags Int64_overflow.pred
  | Incq -> set_flags Int64_overflow.succ
  | _ -> () 
  
let rec set_cc_binary_ALU (m:mach) (result:quad) (op:opcode) (src_val:quad) (dst_val:quad) = 
  let amt = Int64.to_int src_val in
  let set_flags (overflow_op: quad -> quad -> Int64_overflow.t) = 
    begin
      let result_t : Int64_overflow.t = overflow_op dst_val src_val in
      m.flags.fo <- result_t.overflow;
      m.flags.fs <- result < 0L;
      m.flags.fz <- result = 0L
    end in
  match op with
  | Addq -> set_flags Int64_overflow.add  
  | Subq -> set_flags Int64_overflow.sub
  | Imulq -> set_flags Int64_overflow.mul
  | Andq -> 
    begin
      m.flags.fo <- false;
      m.flags.fs <- result < 0L;
      m.flags.fz <- result = 0L
    end 
  | Orq -> set_cc_binary_ALU m result Andq src_val dst_val
  | Xorq -> set_cc_binary_ALU m result Andq src_val dst_val
  | Cmpq -> set_cc_binary_ALU m result Subq src_val dst_val
  | Sarq -> if amt <> 0 then 
    begin
      m.flags.fs <- result < 0L;
      m.flags.fz <- result = 0L;
      m.flags.fo <- amt = 1
    end
  | Shlq -> if amt <> 0 then 
    begin
      m.flags.fs <- result < 0L;
      m.flags.fz <- result = 0L;
      if amt = 1 then
        begin 
          let mask = Int64.shift_left 0xcL 60 in
          let msb_bit_pair = Int64.shift_right_logical (Int64.logand dst_val mask) 60 in 
          m.flags.fo <- msb_bit_pair = 2L || msb_bit_pair = 1L 
        end
    end
  | Shrq -> if amt <> 0 then
    let get_MSB (num:quad) = num < 0L in
    begin
      m.flags.fs <- get_MSB result;
      m.flags.fz <- result = 0L;
      if amt = 1 then
        m.flags.fo <- get_MSB dst_val
    end
  | _ -> ()

let update_state_binary_ALU (op:opcode) (operands : operand list) (m:mach) :unit =
  let src_val = read m (List.hd operands) in
  let dst_loc = List.hd (List.rev operands) in
  let dst_val = read m dst_loc in
  let apply_binary_op (op:quad -> quad -> quad) : quad = op dst_val src_val in
  let amt = Int64.to_int src_val in
  let apply_shift_op (op:quad -> int -> quad) : quad = op dst_val amt in
  let result = 
    begin 
      match op with
      | Addq -> apply_binary_op Int64.add  
      | (Subq | Cmpq) -> apply_binary_op Int64.sub
      | Imulq -> apply_binary_op Int64.mul
      | Andq -> apply_binary_op Int64.logand
      | Orq -> apply_binary_op Int64.logor
      | Xorq -> apply_binary_op Int64.logxor
      | Sarq -> apply_shift_op Int64.shift_right
      | Shlq -> apply_shift_op Int64.shift_left
      | Shrq -> apply_shift_op Int64.shift_right_logical
      | _ -> raise X86lite_segfault
    end in
  begin
    if not (op = Cmpq) then write m dst_loc result;
    set_cc_binary_ALU m result op src_val dst_val
  end   

let update_state_unary_ALU (op:opcode) (operands : operand list) (m:mach) :unit =
  let dst_loc = List.hd operands in
  let dst_val = read m dst_loc in 
  let apply_unary_op (op:quad -> quad) : quad = op dst_val in 
  let result = 
    begin
      match op with
      | Negq -> apply_unary_op Int64.neg 
      | Incq -> apply_unary_op Int64.succ
      | Decq -> apply_unary_op Int64.pred
      | Notq -> apply_unary_op Int64.lognot
      | _ -> raise X86lite_segfault
    end in
  begin
    write m dst_loc result;
    set_cc_unary_ALU m result op dst_val
  end

(* Simulates one step of the machine:
    - fetch the instruction at %rip
    - compute the source and/or destination information from the operands
    - simulate the instruction semantics
    - update the registers and/or memory appropriately
    - set the condition flags
*)
let step (m:mach) : unit =
  let rip_val = read m (Reg Rip) in
  let ins = List.hd (read_quad_from_mem (m.mem) rip_val) in
  let op = get_op ins in
  let operands = get_operands ins in 
  let is_ALU = function
    Set _ | Leaq | Movq | Pushq | Popq | Jmp | Callq | Retq | J _ -> false
    | _ -> true in
  if is_ALU op then
    if List.length operands = 2 then
      update_state_binary_ALU op operands m
    else 
      update_state_unary_ALU op operands m
  else
    update_state_non_ALU op operands m;
  if (read m (Reg Rip)) = rip_val then 
  write m (Reg Rip) (Int64.add 8L rip_val)
  
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

let separate (p:prog) : (prog * prog) =
  let ( ** ) g f : ('a -> 'b) = fun x -> g (f x) in
  let is_text (elem:X86.elem) : bool = 
      match elem with
      | {lbl = _ ; global = _ ; asm = Text _} -> true
      | _ -> false
  in
  (List.filter is_text p, List.filter (not ** is_text) p)

(* given a text_segment, computes how many bytes all the instructions will eventually take up.
   Recall that each instruction takes up exactly 8 bytes, so we just count how many instructions there are times 8. *)
let get_text_size (text:prog) : quad =
  let get_no_instr (elem:X86.elem) : int =
    match elem with
    | {lbl = _ ; global = _ ; asm = Text ins_list} -> List.length ins_list 
    | _ -> 0 
  in
  let instr_per_elem = List.map get_no_instr text in
  Int64.mul 8L (Int64.of_int (List.fold_right (fun x y -> x + y) instr_per_elem 0))

let construct_sym_tab (segment:prog) (bottom:quad) : (lbl * quad) list =
  let data_size (data:data) : int =
    match data with
    | Asciz str -> String.length str + 1
    | Quad _ -> 8
  in
  let len_list (elem:elem) : (lbl * int) =
    match elem with 
    | {lbl = label ; global = _ ; asm = Data data_list} -> (label, List.fold_left (fun acc data -> acc + data_size data) 0 data_list) 
    | {lbl = label ; global = _ ; asm = Text ins_list} -> (label, List.length ins_list * 8) 
    | _ -> raise X86lite_segfault
  in
  let lbl_length_pairs = List.map len_list segment in 
  let labels_list = fst (List.split lbl_length_pairs) in
  let lengths_list = snd (List.split lbl_length_pairs) in
  let prefix_sums = snd (List.fold_left_map (fun acc x -> (acc + x, acc + x)) 0 lengths_list) in
  let shifted_prefix_sums = 0::(List.rev (List.tl (List.rev prefix_sums))) in
  let offsets_from_bottom = List.map (fun x -> Int64.add (Int64.of_int x) bottom) shifted_prefix_sums in
  List.combine labels_list offsets_from_bottom 

let make_sym_tab_func sym_tab =
  function (key:lbl) ->
    let filtered_keys = List.filter (fun (k,_) -> key = k) sym_tab in
    match filtered_keys with
    | [] -> raise (Undefined_sym key)
    | (_,v)::_ -> v

let replace_labels (ins_list:ins list) (symbol_table : (lbl -> quad)) : ins list =
  let replace_label (ins:ins) : ins =
    let replace_operands (operand:operand) :operand =
      match operand with 
      | Imm (Lbl lbl) -> Imm (Lit (symbol_table lbl))
      | Ind1 (Lbl lbl) -> Ind1 (Lit (symbol_table lbl))
      | Ind3 (Lbl lbl, reg) -> Ind3 (Lit (symbol_table lbl), reg)
      | x -> x
    in
    match ins with
    | op, oper_list -> op, List.map replace_operands oper_list
  in
  List.map replace_label ins_list 

let assemble (p:prog) : exec =
  let text_data_pair = separate p in 
  let text_segment = fst text_data_pair in 
  let data_segment = snd text_data_pair in
  let text_size = get_text_size text_segment in
  let data_pos = Int64.add mem_bot text_size in
  let text_pos = mem_bot in
  let data_symbols = construct_sym_tab data_segment data_pos in
  let text_symbols = construct_sym_tab text_segment text_pos in
  let sym_tab = data_symbols @ text_symbols in 
  let symbol_table = make_sym_tab_func sym_tab in
  let get_data_list (elem:elem) =
    match elem with
    | {lbl = _ ; global = _ ; asm = Data data_list} -> data_list
    | _ -> raise X86lite_segfault
  in
  let get_ins_list (elem:elem) =
    match elem with
    | {lbl = _ ; global = _ ; asm = Text ins_list} -> ins_list
    | _ -> raise X86lite_segfault
  in
  let concat_ins_list = List.concat_map get_ins_list text_segment in
  let concat_data_list = List.concat_map get_data_list data_segment in
  let replaced_labels_ins_list = replace_labels concat_ins_list symbol_table in
  let text_seg = List.concat_map sbytes_of_ins replaced_labels_ins_list in
  let data_seg = List.concat_map sbytes_of_data concat_data_list in
  { entry = symbol_table "main"; 
    text_pos = text_pos;
    data_pos = data_pos;
    text_seg = text_seg;
    data_seg = data_seg}

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
