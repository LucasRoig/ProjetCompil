(* Analyses of source language statements:
 - predicate: 'statement returns'
 - depth of operand stack for expression evaluation
 - definite assignment
*)

open Lang

(* ************************************************************ *)
(* ****  Statement returns                                 **** *)
(* ************************************************************ *)

(*Attention, ici on force le return a etre la derniere instruction de la branche*)
let rec stmt_returns = function
    Skip -> false
  | Assign(_,_,_) -> false
  | Seq(_,stmt) -> stmt_returns stmt
  | Cond(_,stmt1,stmt2) -> stmt_returns stmt1 && stmt_returns stmt2
  | While(_,stmt) -> stmt_returns stmt
  | CallC(_,_) -> false
  | Return _ -> true
;;

let fundef_returns (Fundefn(Fundecl(_,_,_),_,stmt)) = stmt_returns stmt;;



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
       plus le nombre de parametres*)
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
  (*Les variables globales sont ignorees comme indique dans la doc java *)
  | VarE(_,Var(binding,name)) -> binding == Global || StringSet.mem name a
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
  | Seq(stmt1,stmt2) ->
     let a = defassign_c a stmt1 in
     defassign_c a stmt2
  | Cond(e,stmt1,stmt2) ->
     if defassign_e a e then
       let s1 = defassign_c a stmt1 in
       let s2 = defassign_c a stmt2 in
       StringSet.inter s1 s2
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

let defassign_fundef (Fundefn(Fundecl(_,_,params_list),_,stmt)) =
    let params_names = List.map name_of_vardecl params_list in
    let set = List.fold_right (StringSet.add) params_names  StringSet.empty in
    let _ = defassign_c set stmt in
    ()
;;

(*Une vraie erreur pour stmt_returns serait bien,
Un vrai affichage des erreurs aussi*)
(*Effectue les differentes analyses et renvoie le programme s'il n'y a pas d'erreurs*)
let analyse_prog (Prog(gvds,fdfs)) =
  List.iter (defassign_fundef) fdfs;
  (*stmt_returns*)
  let fun_returns = List.map (fundef_returns) fdfs in
  if List.exists (fun b -> not b) fun_returns then failwith("Une branche de fonction ne contient pas de return")
  else Prog(gvds,fdfs);;
