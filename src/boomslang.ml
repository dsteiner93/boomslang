(* Top-level of the Boomslang compiler: scan & parse the input,
   check the resulting AST and generate an SAST from it, generate LLVM IR,
   and dump the module *)

   type action = Ast | Sast | LLVM_IR | Compile

   let () =
     let action = ref Compile in
     let set_action a () = action := a in
     let speclist = [
       ("-a", Arg.Unit (set_action Ast), "Print the AST");
       ("-s", Arg.Unit (set_action Sast), "Print the SAST");
       ("-l", Arg.Unit (set_action LLVM_IR), "Print the generated LLVM IR");
       ("-c", Arg.Unit (set_action Compile),
         "Check and print the generated LLVM IR (default)");
     ] in  
     let usage_msg = "usage: ./boomslang.native [-a|-s|-l|-c] [file.boom]" in
     let channel = ref stdin in
     Arg.parse speclist (fun filename -> channel := open_in filename) usage_msg;
     
     let lexbuf = Lexing.from_channel !channel in
     let ast = Parser.program Scanner.read_next_token lexbuf in
     let sast = Semant.check ast in
     let m = Codegen.translate sast in
     Llvm_analysis.assert_valid_module m;
     print_string (Llvm.string_of_llmodule m)
