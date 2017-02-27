(* Analyses of source language statements:
 - predicate: 'statement returns'
 - depth of operand stack for expression evaluation
 - definite assignment
*)

open Lang

(* ************************************************************ *)
(* ****  Statement returns                                 **** *)
(* ************************************************************ *)

let rec stmt_returns = function
    Skip -> false
  | Assign(_,_,_) -> false
  | Seq(_,stmt) -> stmt_returns stmt
  | Cond(_,stmt1,stmt2) -> stmt_returns stmt1 && stmt_returns stmt2
  | While(_,stmt) -> stmt_returns stmt
  | CallC(_,_) -> false
  | Return _ -> true
;;



(* ************************************************************ *)
(* ****  Stack depth                                       **** *)
(* ************************************************************ *)



let rec stack_depth_e = function
    Const(_,_) -> 1
  | VarE(_,_) -> 1
  | BinOp(_,_,e1,e2) ->
     max (stack_depth_e e1) ((stack_depth_e e2)+1)
  | IfThenElse(_,e1,e2,e3) ->
     List.fold_left max 2 [stack_depth_e e1;stack_depth_e e2;stack_depth_e e3]
       (* On va empiler 0 pour faire le test apres l'evaluation de e1, on a donc une
          profondeur minimum de 2*)
  | CallE(_,_,e_list) ->
     (*ici lors de l'evaluation d'un parametre on a deja empile tous les parametres precedents
       la profondeur depend donc de l'ordre d'evaluation des parametres. On peut simplifier
       le probleme en considerant que la profondeur est le maximum des profondeurs des parametres
       plus le nombre de parametres moins 1*)
     let d_list = List.map stack_depth_e e_list in
     List.fold_left max 0 d_list + List.length d_list
;;


let rec stack_depth_c = function
    Skip -> 0
  | Assign(_,_,expr) -> stack_depth_e expr
  | Seq(stmt1,stmt2) -> max (stack_depth_c stmt1) (stack_depth_c stmt2)
  | Cond (e,stmt1,stmt2) ->
     (* On va empiler 0 pour faire le test apres l'evaluation de e, on a donc une
        profondeur minimum de 2*)
     List.fold_left max 2 [stack_depth_e e; stack_depth_c stmt1; stack_depth_c stmt2]
  | While(e,stmt) ->
     (*Ici aussi on a besoin d'un minimum de 2 pour faire le test*)
     max 2 (max (stack_depth_e e) (stack_depth_c stmt))
  | CallC(_,e_list) ->
     (*Comme pour l'appel dans une expression, la profondeur depend de l'ordre des parametres*)
     let d_list = List.map stack_depth_e e_list in
     List.fold_left max 0 d_list + List.length d_list
  | Return e -> stack_depth_e e
;;



(* ************************************************************ *)
(* ****  Definite Assignment                               **** *)
(* ************************************************************ *)

module StringSet =
  Set.Make
    (struct type t = string
	    let compare = Pervasives.compare
     end)
;;
let rec defassign_e a = function
    Const(_,_) -> true
  | VarE(_,Var(_,name)) -> StringSet.mem name a
  | BinOp(_,_,e1,e2) -> defassign_e a e1 && defassign_e a e2
  | IfThenElse(_,e1,e2,e3) -> defassign_e a e1 && defassign_e a e2 && defassign_e a e3
  | CallE(_,_,e_list) ->
     List.for_all (defassign_e a) e_list
;;

exception Def_assign_exception;;
let rec defassign_c a = function
  Skip -> a
  | Assign (_,Var(_,name),e) ->
     if defassign_e a e then StringSet.add name a else raise Def_assign_exception
  | Seq(Assign(t,var,e) ,stmt2) ->
     let a = defassign_c a (Assign(t,var,e)) in
     defassign_c a stmt2
  | Seq(_,stmt2) -> defassign_c a stmt2
  | Cond(e,stmt1,stmt2) ->
     if defassign_e a e then
       let _ = defassign_c a stmt1 in
       let _ = defassign_c a stmt2 in
       a
     else raise Def_assign_exception
  | While(e,stmt) ->
     if defassign_e a e then
       let _ = defassign_c a stmt in a
     else raise Def_assign_exception
  | CallC (_,exp_list)->
     let exp_def = List.map (defassign_e a) exp_list in
     if List.exists not exp_def then raise Def_assign_exception
     else a
  | Return e -> if defassign_e a e then a else raise Def_assign_exception;;
