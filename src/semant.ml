(* Semantic checking for the Python++ compiler *)

open Ast
open Sast

module StringMap = Map.Make(String);;

(* Semantic checking of the AST. Returns an SAST if successful,
   throws an exception if something is wrong. *)

(* Add built-in functions *)
let check program =

let built_in_funcs =
  let add map fdecl = StringMap.add fdecl.fname fdecl map in
  List.fold_left add StringMap.empty [{rtype = Primitive Void; fname = "println"; formals = [(Primitive String, "unused")]; body = []}]
in

let rec check_fcall fname actuals =
  if StringMap.mem fname built_in_funcs then SFuncCall(fname, (List.map check_expr actuals)) else raise (Failure("Unimplemented"))
and

check_call = function
  FuncCall(fname, exprs) -> SCall (check_fcall fname exprs)
| _ -> raise (Failure("Unimplemented"))
and

check_expr = function
  StringLiteral(str) -> (Primitive(String), SStringLiteral(str))
| Call(call) -> (Primitive(Void), check_call call)
| _ -> raise (Failure("Unimplemented"))
and

check_stmt = function
  Expr(expr) -> SExpr (check_expr expr)
| _ -> raise (Failure("Unimplemented"))
and

check_p_unit = function
  Stmt(stmt) -> SStmt (check_stmt stmt)
| _ -> raise (Failure("Unimplemented"))
in

List.map check_p_unit program

