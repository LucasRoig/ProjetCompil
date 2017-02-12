(* Typechecking of source programs *)

open Lang
open Analyses

(* Environments *)

type environment =
    {localvar: (vname * tp) list;
     globalvar: (vname * tp) list;
     returntp: tp;
     funbind: fundecl list}
;;

type erreur_de_type =
  | Indefini of string      (* variable utilisee mais non definie *)
  | Conflit of tp * tp (* conflit de types *)
  | Arite of string * int * int     (* mauvais nombre d’arguments *)
;;

exception Erreur_typage of erreur_de_type;;

let cherche_ident liste nom =
  try List.assoc nom liste with Not_found -> raise(Erreur_typage(Indefini(nom)))
;;

let cherche_var_locale env nom = cherche_ident env.localvar nom
and cherche_var_globale env nom = cherche_ident env.globalvar nom
and cherche_fun env nom =
  let rec cherche nom = function
  [] -> raise(Erreur_typage(Indefini(nom)))
    | Fundecl(tp,n,var)::t ->
       if n = nom then Fundecl(tp,n,var)
       else cherche nom t in
  cherche nom env.funbind;;

let verifie_type type_attendu type_reel =
  if type_attendu <> type_reel then
    raise (Erreur_typage(Conflit(type_attendu, type_reel)))
;;

let rec tp_expr env = function
    Const(_,BoolV(b)) -> Const(BoolT,BoolV(b))
  | Const(_,IntV(x)) -> Const(IntT, IntV(x))
  | Const(_,VoidV) -> Const(VoidT,VoidV)
  | VarE(_,Var(binding,name)) ->
     if binding = Local then VarE(cherche_var_locale env name,Var(binding,name))
     else VarE(cherche_var_globale env name,Var(binding,name))
  | BinOp(_,op,arg1,arg2) ->
     let exp1=tp_expr env arg1 and exp2=tp_expr env arg2 in
     let tp1 = tp_of_expr exp1 and tp2 =tp_of_expr exp2 in
     begin match op with
       BArith _  -> verifie_type IntT tp1;
                    verifie_type IntT tp2;
                    BinOp(IntT,op,exp1,exp2)
     | BCompar _ -> verifie_type tp1 tp2;
                    BinOp(BoolT,op,exp1,exp2)
     | BLogic _  -> verifie_type BoolT tp1;
                    verifie_type BoolT tp2;
                    BinOp(BoolT,op,exp1,exp2) end
  | IfThenElse(_,test,t,f) ->
     let e1 = tp_expr env test and e2 = tp_expr env t and e3 = tp_expr env f in
     verifie_type BoolT (tp_of_expr e1);
     verifie_type (tp_of_expr e2) (tp_of_expr e3); (*e2 et e3 doivent avoir le meme type*)
     IfThenElse(tp_of_expr e2,e1,e2,e3)
  | CallE(_,name,args) ->
     let Fundecl(tp,_,argsAttendus) = cherche_fun env name in
     if List.length args <> List.length argsAttendus
     then raise(Erreur_typage(Arite(name,List.length args,List.length argsAttendus)))
     else let argsReels = List.map (tp_expr env) args in
          List.iter2 verifie_type (List.map tp_of_vardecl argsAttendus) (List.map tp_of_expr argsReels);
          CallE(tp,name,argsReels)
;;

(* TODO: put your definitions here *)
let tp_prog (Prog (gvds, fdfs)) =
  Prog([],
       [Fundefn (Fundecl (BoolT, "even", [Vardecl (IntT, "n")]), [], Skip)])
;;
