(* Semantic checking for the MicroC compiler *)

open Ast
open Sast

module StringMap = Map.Make(String)

(* Semantic checking of the AST. Returns an SAST if successful,
   throws an exception if something is wrong.

   Check each global variable, then check each function *)

let check spunits =
   [(Void, ("println", [(2)]))]
 
 (*
  (**** Check functions ****)

  (* Collect function declarations for built-in functions: no bodies *)
  let built_in_decls = 
    let add_bind map (name, ty) = StringMap.add name {
      typ = Void;
      fname = name; 
      formals = [(ty, "x")];
      locals = []; body = [] } map
    in List.fold_left add_bind StringMap.empty [ ("print", Int);
			                         ("printb", Bool);
			                         ("printf", Float);
			                         ("printbig", Int) ]
    in (* body of check_function *)
    { styp = Void;
      sfname = print;
      sformals = string;
      slocals  = "Hello World";
      sbody = 
    }
  in (List.map check)_function functions)
*)