open Parsing;;
open Lexing;;
open Lambda;;
open Parser;;
open Lexer;;
open Pp;;

exception Not_Ending;;

(* will not stop until receiving ;; *)
let rec get_exp s = 
  let rec check_exp l p = match l with
    | ""::[]    -> raise (Not_Ending) (* when the expresion ends with ; (not with ;;)*)
    | []        -> raise (Not_Ending)
    | ""::t     -> List.rev p
    | h::t      -> check_exp t (h::p)
  in try 
    check_exp (String.split_on_char ';' s) []
  with 
    (* next line *)
    Not_Ending -> get_exp (s^" "^(read_line ()))
;;

(* Tokenizing and evaluating a list of expresions (strings) *)
let rec exec exp ctx = match exp with
  | [] -> ctx
  | h::t -> 
      match s token (from_string (h)) with
        | EvalOfTerm tm  ->
            let ty = typeof ctx tm in
            let tm' = eval ctx tm in
            pp_term_eval ty tm';
            exec t ctx
        | EvalOfType ty  ->
            let ty' = convert_type ctx ty in
            pp_type_eval ty';
            exec t ctx
        | BindOfTerm (name,tm) -> 
            let ty = typeof ctx tm and tm' = eval ctx tm in
            pp_term_bind name ty tm';
            (* Updating Context *)
            exec t (addbinding ctx name (typeof ctx tm) tm')
        | BindOfType (name, ty) ->
            let ty' = convert_type ctx ty in
            pp_type_bind name ty';
            (* Updating Context *)
            exec t (addbinding_type ctx name ty)
;;


let top_level_loop () =
  print_endline "Evaluator of lambda expressions...";
  let rec loop ctx =
    print_string ">> ";
    flush stdout;
    try
      loop (exec (get_exp (read_line ())) ctx);
    with
       Lexical_error ->
         print_endline "lexical error";
         loop ctx
     | Parse_error ->
         print_endline "syntax error";
         loop ctx
     | Type_error e ->
         print_endline ("type error: " ^ e);
         loop ctx
     | End_of_file ->
         print_endline "...bye!!!"
  in
    loop emptyctx
  ;;

(* Program Init *)
top_level_loop ()
;;

