
type ty =
    TyBool
  | TyNat
  | TyString
  | TyArr of ty * ty
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
;;

type command =
    Eval of term
  | Bind of string * term
;;

type context =
  (string * ty * term option) list
;;

val emptyctx : context;;
val addbinding : context -> string -> ty -> term -> context;;
val addbinding_type : context -> string -> ty -> context;;
val getbinding_type : context -> string -> ty;;
val getbinding_term : context -> string -> term;;


val string_of_ty : ty -> string;;
exception Type_error of string;;
val typeof : context -> term -> ty;;



val string_of_term : term -> string;;
exception NoRuleApplies;;
val eval : context -> term -> term;;
exception Not_Found of string;;
