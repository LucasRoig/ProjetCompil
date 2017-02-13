(* Compilation functions *)

open Lang
open Analyses
open Instrs

(* ************************************************************ *)
(* **** Compilation of expressions / statements            **** *)
(* ************************************************************ *)
let position e list =
  let rec pos x e = function
      h::t -> if h = e then x else pos (x+1) e t
    | [] -> failwith "position : l'element n'est pas dans la liste, le typage ne fonctionne pas" in
  pos 0 e list;;

let rec gen_exp varList = function
    Const(IntT,v) -> [Loadc(IntT,v)]
  | Const(BoolT,BoolV t) ->
     if t then [Loadc(IntT, IntV 1)](*true est remplace par 1*)
     else [Loadc(IntT, IntV 0)] (*false par 0*)
  | VarE(_,Var(binding,name)) -> [Loadv(IntT, position name varList)]
  | BinOp(_,op,exp1,exp2) ->
     let l1 = gen_exp varList exp1 and l2 = gen_exp varList exp2 in
     begin match op with
       BArith _ | BLogic _ -> l1@l2@[Bininst(IntT,op)]
     | BCompar _ ->  failwith "gen_exp: une comparaison necessite un branchement et n'est pas encore implemente"
     end
  | IfThenElse(_,exp1,exp2,exp3) ->
     failwith "gen_exp: IfThenElse necessite un branchement et n'est pas encore implemente"
  | CallE(t,name,expList) ->
     failwith "l'appel de fonction n'est pas encore implemente"
  | Const (VoidT,_) ->
     failwith "Une constante ne devrait pas avoir pour type void, le typage ne marche pas"
;;


(* ************************************************************ *)
(* **** Compilation of methods / programs                  **** *)
(* ************************************************************ *)

let gen_prog (Prog (gvds, fdfs)) =
  JVMProg ([],
           [Methdefn (Methdecl (IntT, "even", [IntT]),
                      Methinfo (3, 1),
                      [Loadc (IntT, IntV 0); ReturnI IntT])])

