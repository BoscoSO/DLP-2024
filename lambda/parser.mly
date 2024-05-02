
%{
  open Lambda;;
%}

%token LAMBDA
%token TYPE
%token TRUE
%token FALSE
%token IF
%token THEN
%token ELSE
%token SUCC
%token PRED
%token ISZERO
%token LET
%token LETREC
%token IN
%token CONCAT
%token FIRST
%token REST
%token BOOL
%token NAT
%token STRING
%token FIX
%token LPAREN
%token RPAREN
%token LBRACE
%token RBRACE
%token COMMA
%token DOT
%token EQ
%token COLON
%token ARROW

%token LANGLE
%token RANGLE
%token AS
%token ADD
%token ABS
%token EOF

%token <int> INTV
%token <string> IDV
%token <string> IDT
%token <string> STRINGV

%start s
%type <Lambda.command> s
%type <Lambda.term> term
%type <Lambda.ty> ty

%%

s :
    term EOF
        { EvalOfTerm $1 }
    | TYPE ty EOF
        { EvalOfType $2 }
    | IDV EQ term EOF
        { BindOfTerm ($1, $3) }
    | IDT EQ ty EOF
        { BindOfType ($1, $3) }
    | LET IDV EQ term EOF
        { BindOfTerm ($2, TmLetIn($2, $4, TmVar $2)) }
    | LETREC IDV COLON ty EQ term EOF
        { BindOfTerm ($2, TmLetIn($2, TmFix(TmAbs ($2, $4, $6)), TmVar $2)) }


term :
    appTerm
      { $1 }
  | IF term THEN term ELSE term
      { TmIf ($2, $4, $6) }
  | LAMBDA IDV COLON ty DOT term
      { TmAbs ($2, $4, $6) }
  | LET IDV EQ term IN term
      { TmLetIn ($2, $4, $6) }
  | LETREC IDV COLON ty EQ term IN term
      { TmLetIn ($2, TmFix (TmAbs ($2, $4, $6)), $8) }

appTerm :
    atomicTerm
      { $1 }
  | SUCC atomicTerm
      { TmSucc $2 }
  | PRED atomicTerm
      { TmPred $2 }
  | ISZERO atomicTerm
      { TmIsZero $2 }
  | CONCAT atomicTerm atomicTerm
      { TmConcat ($2, $3) }
  | FIRST atomicTerm
      { TmFirst $2 }
  | REST atomicTerm
      { TmRest $2 }
  | FIX atomicTerm
      { TmFix $2 }
  | appTerm atomicTerm
      { TmApp ($1, $2) }
  | LANGLE IDV EQ term RANGLE AS IDT
      { TmLabel ($2, $4, $7)}

atomicTerm :
    LPAREN term RPAREN
      { $2 }
  | TRUE
      { TmTrue }
  | FALSE
      { TmFalse }
  | IDV
      { TmVar $1 }
  | STRINGV
      { TmString $1 }
  | INTV
      { let rec f = function
            0 -> TmZero
          | n -> TmSucc (f (n-1))
        in f $1 }
  | LBRACE tuple_list RBRACE
      { TmTuple $2 }
  | LBRACE RBRACE
      { TmRecord []}
  | LBRACE record_list RBRACE
      { TmRecord $2 }

tuple_list :
    term
      { [$1] }
  | tuple_list COMMA term
      { $1 @ [$3] }

record_list :
    record_field
      { [$1] }
  | record_list COMMA record_field
      { $1 @ [$3] }

record_field:
    IDV EQ term
      { ( $1, $3 ) }

ty :
    atomicTy
      { $1 }
  | atomicTy ARROW ty
      { TyArr ($1, $3) }

atomicTy :
    LPAREN ty RPAREN  
      { $2 } 
  | BOOL
      { TyBool }
  | NAT
      { TyNat }
  | STRING 
      { TyString }
  | IDT
      { TyDeclared $1 }

  | LBRACE tuple_type_list RBRACE
      { TyTuple ($2) }
  | LBRACE RBRACE
      { TyRecord ([]) }
  | LBRACE record_type_list RBRACE
      { TyRecord ($2) }
  | LANGLE variant_type_list RANGLE
      { TyVariant ($2) }

tuple_type_list :
  ty
    {[$1]}
  | ty COMMA tuple_type_list
    {$1 :: $3}

record_type_list :
    IDV COLON ty
    {[($1, $3)]}
  | IDV COLON ty COMMA record_type_list
    {($1, $3) :: $5}

variant_type_list :
    IDV COLON ty
        {[($1, $3)]}
  | IDV COLON ty COMMA variant_type_list
        {($1, $3) :: $5}