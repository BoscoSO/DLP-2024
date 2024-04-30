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
	| TyList ty -> print_string (string_of_ty ty ^ " list")

let pp_type sn ty =
	open_box 1;
	print_string (" - : type '" ^ sn ^ "' = ");
	pp_type_aux sn ty;
	close_box();
	print_newline();
	print_flush();
