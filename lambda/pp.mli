open Lambda;;

val pp_type : string -> ty -> unit;;
val pp_term_bind : string -> ty -> term -> unit;;
val pp_term_eval : term -> unit;;