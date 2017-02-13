(* Compilation functions *)

open Lang
open Analyses
open Instrs

let compte_labels = ref 0;;
let incr_cpt_label () =
  incr compte_labels;[!compte_labels];; (*On a apparement besoin d'une liste d'int*)

(* ************************************************************ *)
(* **** Compilation of expressions / statements            **** *)
(* ************************************************************ *)
let position e list =
  let rec pos x e = function
      h::t -> if h = e then x else pos (x+1) e t
    | [] -> failwith "position : l'element n'est pas dans la liste, le typage ne fonctionne pas" in
  pos 0 e list;;

(*Le cas des variables globales est a gerer*)
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
     | BCompar opComp ->
        let lbl_true = incr_cpt_label () and lbl_fin = incr_cpt_label () in
        l1@l2@If(opComp,lbl_true)::Loadc(IntT,IntV 0)::Goto lbl_fin::Label lbl_true::Loadc(IntT,IntV 1)::[Label lbl_fin]
     end
  | IfThenElse(_,exp1,exp2,exp3) ->
     let lbl_false = incr_cpt_label () and lbl_fin = incr_cpt_label () in
     (gen_exp varList exp1)@
     [Loadc(IntT,IntV 0);If(BCeq,lbl_false)]@
     (gen_exp varList exp2)@[Goto lbl_fin]@
     (Label lbl_false::(gen_exp varList exp3)@[Label lbl_fin])
  | CallE(t,name,expList) ->
     let push_args = List.concat (List.map (gen_exp varList) expList) in
     let ret_type = if t = BoolT then IntT else t in
     let args_tps = List.map (function BoolT -> IntT | t -> t) (List.map tp_of_expr expList) in
     push_args@[Invoke(ret_type,name,args_tps)]
  | _ ->
     failwith "Le filtrage de gen_exp n'as pas fonctionne, il y a certainement une erreur de typage"
;;


(* ************************************************************ *)
(* **** Compilation of methods / programs                  **** *)
(* ************************************************************ *)

let gen_prog (Prog (gvds, fdfs)) =
  JVMProg ([],
           [Methdefn (Methdecl (IntT, "even", [IntT]),
                      Methinfo (3, 1),
                      [Loadc (IntT, IntV 0); ReturnI IntT])])

