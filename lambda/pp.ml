open Format;;
open Lambda;;
(* Pretty Printer *)

let rec pp_type_aux sn = function
	| TyBool -> print_string "Bool"
	| TyNat -> print_string "Nat"
	| TyString -> print_string "String"
	| TyArr (ty1, ty2) ->
		open_box 1;
			pp_type_aux "" ty1;
		close_box();
		print_string " -> ";
		open_box 1;
			pp_type_aux "" ty2;
		close_box();
	| TyDeclared s -> print_string s;
	| TyList ty ->
		print_string "'";
		pp_type_aux "" ty;
		print_string "' list";
;;
let pp_type sn ty =
	open_box 0;
	print_string (" - : type '" ^ sn ^ "' = ");
	print_space ();
	pp_type_aux sn ty;
	close_box();
	print_newline();
	print_flush();
;;

let pp_list l =	print_string "[lista]";;

let rec pp_term_aux = function
	| TmTrue -> print_string "True "
	| TmFalse -> print_string "False "
	| TmZero -> print_string "0 "
	| TmString s -> print_string ("\"" ^ s ^ "\" ");
	| TmVar s -> print_string (s)
	| TmIf (cond, t, f) ->
		open_box 1;
		print_string "if ";
		pp_term_aux cond;
		print_space ();
		print_string "then ";
		pp_term_aux t;
		print_space ();
		print_string "else ";
		pp_term_aux f;
		print_space ();
		close_box ();
	| TmSucc t ->
		open_box 1;
		print_string "succ ";
		pp_term_aux t;
		close_box ();
	| TmPred t -> 
		open_box 1;
		print_string "pred ";
		pp_term_aux t;
		close_box ();
	| TmIsZero n ->  
		open_box 1;
		print_string "iszero ";
		pp_term_aux n;
		close_box ();
	| TmAbs (idv, ty, term) -> 
		open_box 1;
		print_string "λ";
		print_string idv;
		print_string " : ";
		pp_type_aux "" ty;
		print_string ". ";
		print_space();
		pp_term_aux term;
		close_box();
    | TmApp (t1, t2) ->
    	open_box 1;
    	pp_term_aux t1;
    	print_string " (";
    	pp_term_aux t2;
    	print_string ") ";
    	close_box();
  	| TmLetIn (idv, t1, t2) -> 
  		open_box 1;
  		print_string "let ";
  		print_string idv;
  		print_string " = ";
  		print_space();
  		pp_term_aux t1;
  		print_space();
  		print_string " in ";
  		print_space();
  		pp_term_aux t2;
  		close_box ();
  	| TmFix t ->
  		open_box 1;
  		print_string "fix ";
  		pp_term_aux t;
  		close_box();
  	| TmConcat (s1, s2) ->
  		open_box 1;
  		print_string "concat ";
  		print_space();
  		pp_term_aux s1;
  		print_space();
  		pp_term_aux s2;
  		close_box();
  	| TmFirst s ->
  		open_box 1;
  		print_string "first ";
  		print_space();
  		pp_term_aux s;
  		close_box();
  	| TmRest s ->
  		open_box 1;
  		print_string "rest ";
  		print_space();
  		pp_term_aux s;
  		close_box();
 	| TmList (ty, h, t) -> 
 		open_box 1;
 		pp_list (TmList (ty, h, t));
 		close_box (); 
	| TmEmptyList ty-> 
		open_box 1;
		print_string "[] : ";
		pp_type_aux "" ty;
		close_box();
 	| TmIsEmptyList (ty, l) ->
 		open_box 1;
 		print_string "isemptylist ";
 		print_space();
 		pp_term_aux l;
 		print_space();
 		print_string " : ";
 		pp_type_aux "" ty;
 		close_box();
  	| TmHead (ty, l) -> 
  		open_box 1;
  		print_string "head ";
  		print_space();
  		pp_list l;
  		close_box();
  	| TmTail (ty, l) ->
  		open_box 1;
  		print_string "tail ";
  		print_space();
  		pp_list l;
  		close_box();
;;

let pp_term_bind sn ty term =
	open_box 0;
	print_string " - : val '"; 
	print_string sn; 
	print_string "' : ";
	pp_type_aux sn ty;
	print_string " = ";
	print_space();
	pp_term_aux term;
	close_box();
	print_newline();
	print_flush()
;;

let pp_term_eval term =
	open_box 0;
	print_string " - : val = ";
	print_space();
	pp_term_aux term;
	close_box();
	print_newline();
	print_flush()
;;