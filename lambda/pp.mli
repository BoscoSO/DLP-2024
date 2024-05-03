open Lambda;;

val pp_type_bind : string -> ty -> unit;;
val pp_type_eval : ty -> unit;;
val pp_term_bind : string -> ty -> term -> unit;;
val pp_term_eval : ty -> term -> unit;;