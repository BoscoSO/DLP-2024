open Option;;

(* TYPE DEFINITIONS *)

type ty =
    TyBool
  | TyNat
  | TyString
  | TyArr of ty * ty
  | TyDeclared of string
  | TyList of ty
;;



type term =
    TmTrue
  | TmFalse
  | TmIf of term * term * term
  | TmZero
  | TmSucc of term
  | TmPred of term
  | TmIsZero of term
  | TmVar of string
  | TmAbs of string * ty * term
  | TmApp of term * term
  | TmLetIn of string * term * term
  | TmFix of term
  | TmString of string
  | TmConcat of term * term
  | TmFirst of term
  | TmRest of term
  | TmList of ty * term * term 
  | TmEmptyList of ty
  | TmIsEmptyList of ty * term 
  | TmHead of ty * term 
  | TmTail of ty * term
;;

(* Command *)
type command =
  | EvalOfTerm of term
  | EvalOfType of ty
  | BindOfTerm of string * term
  | BindOfType of string * ty
;;

type binding =
  | BindTy of ty
  | BindTm of (ty * term)
;;

(* CONTEXT MANAGEMENT *)
type context =
  (string * binding) list
;;

let emptyctx =
  []
;;

(* Adds binding to a given context *)
let addbinding ctx x ty tm =
  (x, BindTm(ty, tm)) :: ctx
;;

let addbinding_type ctx x ty =
  (x, BindTy ty) :: ctx
;;

exception Not_Found of string;;

(* Gets binding to a given context *)
let getbinding_type ctx x = match List.assoc x ctx with
    BindTy ty -> ty
  | BindTm (ty, _) -> ty
;;

let getbinding_term ctx x = match List.assoc x ctx with
    BindTm (_, tm) -> tm
  | _ -> raise (Not_Found x)
;;


(* TYPE MANAGEMENT (TYPING) *)

let rec string_of_ty ty = match ty with
    TyBool ->
      "Bool"
  | TyNat ->
      "Nat"
  | TyString ->
      "String"
  | TyArr (ty1, ty2) ->
      "(" ^ string_of_ty ty1 ^ ")" ^ " -> " ^ "(" ^ string_of_ty ty2 ^ ")"
  | TyDeclared str ->
      str
  | TyList t ->
      string_of_ty t ^ " list"
;;

let rec convert_type ctx ty = match ty with
    TyBool ->
      TyBool
  | TyNat ->
      TyNat
  | TyString ->
      TyString
  | TyArr (t1, t2) ->
      TyArr (convert_type ctx t1, convert_type ctx t2)
  | TyDeclared ty ->
      getbinding_type ctx ty
  | TyList ty ->
      TyList (convert_type ctx ty)
;;

exception Type_error of string
;;

let rec typeof ctx tm = match tm with
    (* T-True *)
    TmTrue ->
      TyBool

    (* T-False *)
  | TmFalse ->
      TyBool

    (* T-If *)
  | TmIf (t1, t2, t3) ->
      if typeof ctx t1 = TyBool then
        let tyT2 = typeof ctx t2 in
        if typeof ctx t3 = tyT2 then tyT2
        else raise (Type_error "arms of conditional have different types")
      else
        raise (Type_error "guard of conditional not a boolean")
      
    (* T-Zero *)
  | TmZero ->
      TyNat

    (* T-Succ *)
  | TmSucc t1 ->
      if typeof ctx t1 = TyNat then TyNat
      else raise (Type_error "argument of succ is not a number")

    (* T-Pred *)
  | TmPred t1 ->
      if typeof ctx t1 = TyNat then TyNat
      else raise (Type_error "argument of pred is not a number")

    (* T-Iszero *)
  | TmIsZero t1 ->
      if typeof ctx t1 = TyNat then TyBool
      else raise (Type_error "argument of iszero is not a number")

    (* T-Var *)
  | TmVar x ->
      (try getbinding_type ctx x with
       _ -> raise (Type_error ("no binding type for variable " ^ x)))

    (* T-Abs *)
  | TmAbs (x, tyT1, t2) ->
      let ty' = convert_type ctx tyT1 in
      let ctx' = addbinding_type ctx x ty' in
      let tyT2 = typeof ctx' t2 in
      TyArr (ty', tyT2)

    (* T-App *)
  | TmApp (t1, t2) ->
      let tyT1 = typeof ctx t1 in
      let tyT2 = typeof ctx t2 in
      (match tyT1 with
           TyArr (tyT11, tyT12) ->
             if tyT2 = tyT11 then tyT12
             else

                raise (Type_error ("parameter type mismatch"))
         | _ -> raise (Type_error "arrow type expected"))

    (* T-Let *)
  | TmLetIn (x, t1, t2) ->
      let tyT1 = typeof ctx t1 in
      let ctx' = addbinding_type ctx x tyT1 in
      typeof ctx' t2

    (* T-Fix*)
  | TmFix t1 ->
      let tyT1 = typeof ctx t1 in 
      (match tyT1 with
        TyArr (tyT11, tyT12) ->
          if tyT11 = tyT12 then tyT12
          else raise (Type_error "result of body not compatible with domain")
          | _ -> raise (Type_error "arrow type expected"))

    (* String rules *)
  | TmString _ ->
      TyString
  | TmConcat (t1, t2)->
      let tyT1 = typeof ctx t1 in 
      let tyT2 = typeof ctx t2 in 
      (match (tyT1, tyT2) with
          (TyString, TyString) -> TyString
        | (_, TyString) -> raise (Type_error "first argument of concat is not a string")
        | (TyString, _) -> raise (Type_error "second argument of concat is not a string")
        | (_, _) -> raise (Type_error "none of the arguments of concat are strings"))
  | TmFirst s ->
      if typeof ctx s = TyString then TyString
      else raise (Type_error "argument of 'first' is not a string")
  | TmRest s ->
      if typeof ctx s = TyString then TyString
      else raise (Type_error "argument of 'rest' is not a string")
    (* List Rules *)
    (* T-Nil *)
  | TmEmptyList t ->
      TyList t 
    (* T-Cons *)
  | TmList (ty, h, t) ->
      let tyHD = typeof ctx h in 
      let tyTL = typeof ctx t in
      if (tyHD = ty) && (tyTL = TyList ty) then TyList ty
      else raise (Type_error ("type mismatch in elements of " ^ string_of_ty ty ^ " list"))
    (* T-IsNil *)
  | TmIsEmptyList (ty, t) ->
      if typeof ctx t = TyList ty then TyBool
      else raise (Type_error "argument of 'isEmptyList' is not a list")
    (* T-Head *)
  | TmHead (ty, t) ->
      if typeof ctx t = TyList ty then ty
      else raise (Type_error "argument of 'head' is not a list")
    (* T-Tail *)
  | TmTail (ty, t) ->
      if typeof ctx t = TyList ty then TyList ty
      else raise (Type_error "argument of 'tail' is not a list")

;;


(* TERMS MANAGEMENT (EVALUATION) *)

let rec string_of_term = function
    TmTrue ->
      "true"
  | TmFalse ->
      "false"
  | TmIf (t1,t2,t3) ->
      "if " ^ "(" ^ string_of_term t1 ^ ")" ^
      " then " ^ "(" ^ string_of_term t2 ^ ")" ^
      " else " ^ "(" ^ string_of_term t3 ^ ")"
  | TmZero ->
      "0"
  | TmSucc t ->
     let rec f n t' = match t' with
          TmZero -> string_of_int n
        | TmSucc s -> f (n+1) s
        | _ -> "succ " ^ "(" ^ string_of_term t ^ ")"
      in f 1 t
  | TmPred t ->
      "pred " ^ "(" ^ string_of_term t ^ ")"
  | TmIsZero t ->
      "iszero " ^ "(" ^ string_of_term t ^ ")"
  | TmVar s ->
      s
  | TmAbs (s, tyS, t) ->
      "(lambda " ^ s ^ ":" ^ string_of_ty tyS ^ ". " ^ string_of_term t ^ ")"
  | TmApp (t1, t2) ->
      "(" ^ string_of_term t1 ^ " " ^ string_of_term t2 ^ ")"
  | TmLetIn (s, t1, t2) ->
      "let " ^ s ^ " = " ^ string_of_term t1 ^ " in " ^ string_of_term t2
  | TmFix t ->
      "(fix " ^ string_of_term t ^ ")"
    (* Strings *)
  | TmString s ->
      "\"" ^ s ^ "\""
  | TmConcat (s1, s2) ->
      string_of_term s1 ^ string_of_term s2
  | TmFirst s ->
      string_of_term s
  | TmRest s ->
      string_of_term s
    (* Lists *)
  | TmEmptyList t ->
      "[]"
  | TmList (ty, h, TmEmptyList t) ->
      "[" ^ string_of_term h ^ "]"
  | TmList (ty, h, t)->
      let rec string_of_list l = match l with
        | TmEmptyList ty-> ""
        | TmList (ty, h, TmEmptyList t) -> string_of_term h
        | TmList (ty, h, t) -> string_of_term h ^ ", " ^ string_of_list t
        | t -> string_of_term t
      in "[" ^ string_of_term h ^ ", " ^ string_of_list t ^ "]"
  | TmIsEmptyList (ty, t) ->
      "IsEmptyList : List [" ^ string_of_ty ty ^ "] : [" ^ string_of_term t ^ "]"
  | TmHead (ty, t) ->
      "Head : " ^ string_of_term t
  | TmTail (ty, t) ->
      let rec string_of_list l = match l with
        | TmEmptyList ty-> ""
        | TmList (ty, h, TmEmptyList t) -> string_of_term h
        | TmList (ty, h, t) -> string_of_term h ^ ", " ^ string_of_list t
        | t -> string_of_term t
      in "Tail : [" ^ string_of_list t ^ "]"
;;
(***********************************-EVAL-***********************************)

let rec ldif l1 l2 = match l1 with
    [] -> []
  | h::t -> if List.mem h l2 then ldif t l2 else h::(ldif t l2)
;;

let rec lunion l1 l2 = match l1 with
    [] -> l2
  | h::t -> if List.mem h l2 then lunion t l2 else h::(lunion t l2)
;;

let rec free_vars tm = match tm with
    TmTrue ->
      []
  | TmFalse ->
      []
  | TmIf (t1, t2, t3) ->
      lunion (lunion (free_vars t1) (free_vars t2)) (free_vars t3)
  | TmZero ->
      []
  | TmSucc t ->
      free_vars t
  | TmPred t ->
      free_vars t
  | TmIsZero t ->
      free_vars t
  | TmVar s ->
      [s]
  | TmAbs (s, _, t) ->
      ldif (free_vars t) [s]
  | TmApp (t1, t2) ->
      lunion (free_vars t1) (free_vars t2)
  | TmLetIn (s, t1, t2) ->
      lunion (ldif (free_vars t2) [s]) (free_vars t1)
  | TmFix t ->
      free_vars t
    (* Strings *)
  | TmString _ ->
      []
  | TmConcat (t1, t2) ->
      lunion (free_vars t1) (free_vars t2)
  | TmFirst s ->
      free_vars s
  | TmRest s ->
      free_vars s
    (* Lists *)
  | TmEmptyList ty -> 
      []
  | TmList (ty, h, t) ->
      lunion (free_vars h) (free_vars t)
  | TmIsEmptyList (ty, t)->
      free_vars t
  | TmHead (ty, t) ->
      free_vars t
  | TmTail (ty, t) ->
      free_vars t
;;

let rec fresh_name x l =
  if not (List.mem x l) then x else fresh_name (x ^ "'") l
;;
    
let rec subst x s tm = match tm with
    TmTrue ->
      TmTrue
  | TmFalse ->
      TmFalse
  | TmIf (t1, t2, t3) ->
      TmIf (subst x s t1, subst x s t2, subst x s t3)
  | TmZero ->
      TmZero
  | TmSucc t ->
      TmSucc (subst x s t)
  | TmPred t ->
      TmPred (subst x s t)
  | TmIsZero t ->
      TmIsZero (subst x s t)
  | TmVar y ->
      if y = x then s else tm
  | TmAbs (y, tyY, t) -> 
      if y = x then tm
      else let fvs = free_vars s in
           if not (List.mem y fvs)
           then TmAbs (y, tyY, subst x s t)
           else let z = fresh_name y (free_vars t @ fvs) in
                TmAbs (z, tyY, subst x s (subst y (TmVar z) t))  
  | TmApp (t1, t2) ->
      TmApp (subst x s t1, subst x s t2)
  | TmLetIn (y, t1, t2) ->
      if y = x then TmLetIn (y, subst x s t1, t2)
      else let fvs = free_vars s in
           if not (List.mem y fvs)
           then TmLetIn (y, subst x s t1, subst x s t2)
           else let z = fresh_name y (free_vars t2 @ fvs) in
                TmLetIn (z, subst x s t1, subst x s (subst y (TmVar z) t2))
  | TmFix t ->
      TmFix (subst x s t)
    (* Strings *)
  | TmString t ->
      TmString t
  | TmConcat (t1, t2) ->
      TmConcat (subst x s t1,subst x s t2)
  | TmFirst t ->
      TmFirst (subst x s t)
  | TmRest t ->
      TmRest (subst x s t)
    (* Lists *)
  | TmEmptyList ty ->
      tm 
  | TmList (ty, h, t) ->
      TmList (ty, (subst x s h), (subst x s t))
  | TmIsEmptyList (ty, t) ->
      TmIsEmptyList (ty, subst x s t)
  | TmHead (ty, t) ->
      TmHead (ty, subst x s t)
  | TmTail (ty, t) ->
      TmTail (ty, subst x s t)
;;

let rec isnumericval tm = match tm with
    TmZero -> true
  | TmSucc t -> isnumericval t
  | _ -> false
;;

let rec isval tm = match tm with
    TmTrue  -> true
  | TmFalse -> true
  | TmAbs _ -> true
  | TmString _ -> true
  | TmEmptyList _ -> true
  | TmList (_, h, t) -> isval h && isval t
  | t when isnumericval t -> true
  | _ -> false
;;

exception NoRuleApplies
;;

let rec eval1 ctx tm = match tm with
    (* E-IfTrue *)
    TmIf (TmTrue, t2, _) ->
      t2

    (* E-IfFalse *)
  | TmIf (TmFalse, _, t3) ->
      t3

    (* E-If *)
  | TmIf (t1, t2, t3) ->
      let t1' = eval1 ctx t1 in
      TmIf (t1', t2, t3)

    (* E-Succ *)
  | TmSucc t1 ->
      let t1' = eval1 ctx t1 in
      TmSucc t1'

    (* E-PredZero *)
  | TmPred TmZero ->
      TmZero

    (* E-PredSucc *)
  | TmPred (TmSucc nv1) when isnumericval nv1 ->
      nv1

    (* E-Pred *)
  | TmPred t1 ->
      let t1' = eval1 ctx t1 in
      TmPred t1'

    (* E-IszeroZero *)
  | TmIsZero TmZero ->
      TmTrue

    (* E-IszeroSucc *)
  | TmIsZero (TmSucc nv1) when isnumericval nv1 ->
      TmFalse

    (* E-Iszero *)
  | TmIsZero t1 ->
      let t1' = eval1 ctx t1 in
      TmIsZero t1'

    (* E-AppAbs *)
  | TmApp (TmAbs(x, _, t12), v2) when isval v2 ->
      subst x v2 t12

    (* E-App2: evaluate argument before applying function *)
  | TmApp (v1, t2) when isval v1 ->
      let t2' = eval1 ctx t2 in
      TmApp (v1, t2')

    (* E-App1: evaluate function before argument *)
  | TmApp (t1, t2) ->
      let t1' = eval1 ctx t1 in
      TmApp (t1', t2)

    (* E-LetV *)
  | TmLetIn (x, v1, t2) when isval v1 ->
      subst x v1 t2

    (* E-Let *)
  | TmLetIn(x, t1, t2) ->
      let t1' = eval1 ctx t1 in
      TmLetIn (x, t1', t2)
    (* E-FixBeta *)
  | TmFix (TmAbs (x, _, t2)) ->
      subst x tm t2
    (* E-Fix *)
  | TmFix t1 ->
      let t1' = eval1 ctx t1 in 
      TmFix t1'
    (* Strings *)
    (* E-Concat *)
  | TmConcat (TmString s1, TmString s2) ->
      TmString (s1 ^ s2)
    (* E-Concat *)
  | TmConcat (TmString s1, t2) ->
      let t2' = eval1 ctx t2 in 
      TmConcat (TmString s1, t2')
    (* E-Concat *)
  | TmConcat (t1, t2) ->
      let t1' = eval1 ctx t1 in 
      TmConcat (t1', t2)
    (* E-First1 *)
  | TmFirst (TmString s) ->
      if String.length s < 1 then TmString ""
      else TmString (String.make 1 s.[0])
    (* E-First2 *)
  | TmFirst s ->
      let s' = eval1 ctx s in
      TmFirst s'
    (* E-Rest1 *)
  | TmRest (TmString s) ->
      if String.length s < 2 then TmString ""
      else TmString (String.sub s 1 ((String.length s)-1))
    (* E-Rest2 *)
  | TmRest s ->
      let s' = eval1 ctx s in
      TmRest s'
    (* Lists *)
    (* E-Cons2 *)
  | TmList (ty, h, t) when isval h ->
      TmList (ty, h, eval1 ctx t)
    (* E-Cons1 *)
  | TmList (ty, h, t) ->
      TmList (ty, eval1 ctx h, t)
    (* E-IsNilNil *)
  | TmIsEmptyList (ty, TmEmptyList _) ->
      TmTrue
    (* E-IsNilCons *)
  | TmIsEmptyList (ty, TmList (_, _, _)) ->
      TmFalse
    (* E-IsNil *)
  | TmIsEmptyList (ty, t) ->
      TmIsEmptyList (ty, eval1 ctx t)
    (* E-HeadCons *)
  | TmHead (ty, TmList(_, h, _)) ->
      h
    (* E-Head *)
  | TmHead (ty, t) ->
      TmHead (ty, eval1 ctx t)
    (* E-TailCons *)
  | TmTail (ty, TmList(_, _, t)) ->
      t
    (* E-Tail *)
  | TmTail (ty, t) ->
      TmTail (ty, eval1 ctx t)

  | TmVar x ->  
      getbinding_term ctx x (* Not necesary to handling error because typeof aldready did it *)
  | _ ->
      raise NoRuleApplies
;;


(* Evaluate until no more terms can be evaluated *)
let rec eval ctx tm =
  try
    let tm' = eval1 ctx tm in
      eval ctx tm' 
  with
    NoRuleApplies -> tm
;;

