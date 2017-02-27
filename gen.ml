(* Compilation functions *)

open Lang
open Analyses
open Instrs

let compte_labels = ref 0;;
let incr_cpt_label () =
  incr compte_labels;[!compte_labels];; (*On a apparement besoin d'une liste d'int*)

(*Remplace un type BoolT en IntT*)
let boolT_to_intT = function
    BoolT -> IntT
  | tp -> tp
;;
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
     let ret_type = boolT_to_intT t in
     let args_tps = List.map boolT_to_intT (List.map tp_of_expr expList) in
     push_args@[Invoke(ret_type,name,args_tps)]
  | _ ->
     failwith "Le filtrage de gen_exp n'as pas fonctionne, il y a certainement une erreur de typage"
;;

let rec gen_stmt varList = function
    Skip -> [Nop]
  | Assign(tp,Var(binding,name),exp)->
     let pos = position name varList in
     (gen_exp varList exp)@[Storev(IntT,pos)]
  | Seq(s1,s2) -> (gen_stmt varList s1)@(gen_stmt varList s2)
  | Cond(test,t,f) ->
     let lbl_false = incr_cpt_label () and lbl_fin = incr_cpt_label () in
     let cond = gen_exp varList test
     and t = gen_stmt varList t
     and f = gen_stmt varList f in
     cond@[Loadc(IntT,IntV 0); If(BCeq,lbl_false)]@t@[Goto lbl_fin;Label lbl_false]@f@[Label lbl_fin]
  | While(test,stmt) ->
     let lbl_test = incr_cpt_label () and lbl_fin = incr_cpt_label () in
     let test = gen_exp varList test
     and stmt = gen_stmt varList stmt in
     [Label lbl_test]@test@[Loadc(IntT,IntV 0);If(BCeq, lbl_fin)]@stmt@[Goto lbl_test;Label lbl_fin]
  | CallC(name,expList) ->
     (*Comment trouver le type de la fonction sans environnement ?*)
     let push_args = List.concat (List.map (gen_exp varList) expList) in
     let args_tps = List.map boolT_to_intT (List.map tp_of_expr expList) in
     push_args@[Invoke(VoidT,name,args_tps)]
  | Return exp ->
     let tp = boolT_to_intT (tp_of_expr exp) in
     (gen_exp varList exp)@[ReturnI tp]
;;

let gen_fundefn (Fundefn (Fundecl(retTp,fname,params), locVars, stmt)) =
  let meth_info = Methinfo(50,50) in (*constantes arbitraires pour l'instant*)
  let meth_decl = Methdecl(boolT_to_intT retTp,fname,List.map boolT_to_intT (List.map tp_of_vardecl params)) in
  let stmt_list = (gen_stmt (List.map name_of_vardecl (params@locVars)) stmt) in
  Methdefn(meth_decl,meth_info,stmt_list)
;;
(* ************************************************************ *)
(* **** Compilation of methods / programs                  **** *)
(* ************************************************************ *)

let gen_prog (Prog (gvds, fdfs)) =
  JVMProg(gvds,List.map gen_fundefn fdfs);;
