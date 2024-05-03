
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
%token ISEMPTYLIST
%token HEAD
%token TAIL
%token BOOL
%token NAT
%token STRING
%token NULL
%token COMMA
%token FIX
%token LPAREN
%token RPAREN
%token LBRACE
%token RBRACE
%token COMMA
%token LBRACK
%token RBRACK
%token DOT
%token EQ
%token COLON
%token ARROW

%token LANGLE
%token RANGLE
%token AS
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
    | ty EOF
        { EvalOfType $1 }
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
  | ABS term
      {TmAbsVal $2}

appTerm :
    pathAtomicTerm
      { $1 }
  | SUCC pathAtomicTerm
      { TmSucc $2 }
  | PRED pathAtomicTerm
      { TmPred $2 }
  | ISZERO pathAtomicTerm
      { TmIsZero $2 }
  | CONCAT pathAtomicTerm pathAtomicTerm
      { TmConcat ($2, $3) }
  | FIRST pathAtomicTerm
      { TmFirst $2 }
  | REST pathAtomicTerm
      { TmRest $2 }
  | FIX pathAtomicTerm
      { TmFix $2 }
  | LBRACK appTerm COMMA appTerm RBRACK COLON ty
      { TmList ($7, $2, $4)}
  | ISEMPTYLIST appTerm COLON ty
      { TmIsEmptyList ($4, $2) }
  | HEAD appTerm COLON ty
      { TmHead ($4, $2) }
  | TAIL appTerm COLON ty
      { TmTail ($4, $2) }
  | LBRACK RBRACK COLON ty 
      { TmEmptyList ($4) }
  | appTerm pathAtomicTerm
      { TmApp ($1, $2) }
  | LANGLE IDV EQ term RANGLE AS IDT
      { TmLabel ($2, $4, $7)}


pathAtomicTerm :
    pathAtomicTerm DOT IDV
    { TmProj ($1, $3) }
  | pathAtomicTerm DOT STRINGV
    { TmProj ($1, $3) }
  | pathAtomicTerm DOT INTV 
    { TmProj ($1, string_of_int $3) }
  | atomicTerm
    { $1 }


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
  | ABS LPAREN term RPAREN
      { TmAbsVal $3 }

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
  | ABS ty
      { TyAbsVal $2 }



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
  | LBRACK ty RBRACK
      { TyList ($2) }
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

