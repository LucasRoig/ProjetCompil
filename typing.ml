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
  | Fundecl_err of string * string (*mauvaise declaration de fonction *)
;;

exception Erreur_typage of erreur_de_type;;
let env_initial funList =
  {localvar = [];
   globalvar = [];
   returntp = VoidT;
   funbind = List.map (function Fundefn(decl,_,_) -> decl ) funList}
;;
let ajout_variable_locale (Vardecl(tp,nom)) env =
  {localvar= (nom,tp)::env.localvar;
   globalvar= env.globalvar;
   returntp= env.returntp;
   funbind= env.funbind}
;;
let ajout_variable_globale (Vardecl(tp,nom)) env =
  {localvar= env.localvar;
   globalvar= (nom,tp)::env.globalvar;
   returntp= env.returntp;
   funbind= env.funbind}
;;

let change_returntp tp env =
  {localvar = env.localvar;
   globalvar = env.globalvar;
   returntp = tp;
   funbind = env.funbind}
;;

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

let cherche_var env nom =
  try (Local,cherche_var_locale env nom)
  with Erreur_typage(Indefini _) -> (Global, cherche_var_globale env nom)
;;

let verifie_type type_attendu type_reel =
  if type_attendu <> type_reel then
    raise (Erreur_typage(Conflit(type_attendu, type_reel)))
;;

let rec tp_expr env = function
    Const(_,BoolV(b)) -> Const(BoolT,BoolV(b))
  | Const(_,IntV(x)) -> Const(IntT, IntV(x))
  | Const(_,VoidV) -> Const(VoidT,VoidV)
  | VarE(_,Var(_,name)) ->
     let (binding,tp) = cherche_var env name in
     VarE(tp,Var(binding,name))
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

let rec tp_stmt env = function
    Skip -> Skip
  | Return exp ->
     let e = tp_expr env exp in
     verifie_type env.returntp (tp_of_expr e);
     Return e (*Que donne return; (sans expression)*)
  | Seq(s1,s2) -> Seq(tp_stmt env s1, tp_stmt env s2)
  | Assign (_,Var(_,nom),exp) ->
     let (binding,varType) = cherche_var env nom in
     let e = tp_expr env exp in
     verifie_type varType (tp_of_expr e);
     Assign(VoidT,Var(binding,nom),e)
  | Cond(test,t,f) ->
     let expTest = tp_expr env test in
     verifie_type BoolT (tp_of_expr expTest);
     Cond(expTest, tp_stmt env t, tp_stmt env f)
  | While(cond,stmt) ->
     let exp = tp_expr env cond in
     verifie_type BoolT (tp_of_expr exp);
     While(exp,tp_stmt env stmt)
  | CallC(name,args) -> (*doit etre de type void*)
     let Fundecl(tp,_,argsAttendus) = cherche_fun env name in
     verifie_type VoidT tp;
     if List.length args <> List.length argsAttendus
     then raise(Erreur_typage(Arite(name,List.length args,List.length argsAttendus)))
     else let argsReels = List.map(tp_expr env) args in
          List.iter2 verifie_type (List.map tp_of_vardecl argsAttendus) (List.map tp_of_expr argsReels);
          CallC(name,argsReels)
;;

let tp_fdefn env (Fundefn(Fundecl(tpRet,name,params),locVars,body)) =
  (*Verifier que le type des parametres et des variables n'est pas void*)
  let params_et_vars = params@locVars in
  if List.exists (function (Vardecl(tp,_)) -> tp = VoidT) params_et_vars
  then raise (Erreur_typage(Fundecl_err(name,"Un parametre ou une variable locale est de type void")))
  else (*Verifier que les parametres et les variables ont un nom different*)
    if List.length params_et_vars != List.length (List.sort_uniq (function a -> function b -> if a=b then 0 else if a > b then 1 else -1) params_et_vars)
    then raise(Erreur_typage(Fundecl_err(name,"Certains parametres ou variables ont le meme nom")))
    else let newEnv = List.fold_right ajout_variable_locale params_et_vars (change_returntp tpRet env) in
         Fundefn(Fundecl(tpRet,name,params),locVars,tp_stmt newEnv body)
;;

(*Faire un joli affichage d'erreurs serait sympa*)
let tp_prog (Prog (gvds, fdfs)) =
(*Verifier que deux variables globales n'ont pas le meme nom*)
  let var_names = List.map name_of_vardecl gvds
  in if List.length var_names != List.length (List.sort_uniq (function a -> function b -> if a=b then 0 else if a>b then 1 else -1) var_names)
    then failwith "Certaines varibales locales ont le meme nom"
    else let env_initial = List.fold_right ajout_variable_globale gvds (env_initial fdfs) in
         Prog(gvds, List.map (tp_fdefn env_initial) fdfs)
;;
