# Lambda Calculus Interpreter
## Programming Languages Design

### 1.- Improved functionalities

### 2.- Added Functionalities
1. __Internal Fixed Point Combinator__
To simplify the declaration of recursive functions, we added a new token to the syntax of the interpreter, 'LETREC'. This token now allows the interpreter to know that the function is recursive and process it as such.
For this, we added a new type of term, 'TmFix'
To prove the correctfullness of the behaviour of this new functionality, we provide, both here and in the examples.txt file, the following implementations of the product, Fibonacci and factorial functions:
```
>> letrec sum : Nat -> Nat -> Nat = 
	lambda n : Nat. lambda m : Nat.
		if iszero n then m
		else succ (sum (pred n) m);;

>> letrec prod : Nat -> Nat -> Nat =
	lambda n : Nat. lambda m : Nat.
		if iszero n then 0
		else if iszero m then 0
			else sum n (prod n (pred m));;

>> letrec fib : Nat -> Nat = 
	lambda n : Nat. 
		if iszero n then 0 
			else if iszero (pred n) then 1 
				else sum (fib (pred n)) (fib (pred (pred n)));;

>> letrec fact: Nat -> Nat =
    lambda n : Nat. 
    	if iszero n then 1 
    	else prod n (fact (pred n));;
```

2. __Global Definitions Context__
Cambios en el lambda.ml
- En el tipo ty añadi el nuevo tipo TyDeclared of string para referirse a los tipos que no son propios del lenguaje y definimos nosotros
- El tipo contexto ahora es una lista de (string * binding)
- El tipo binding puede ser | BindTy of ty | BindTm of (ty * term)
- Esto simplifica las funciones de add y getbinding
En el lexer está IDT y TYPE. IDT es el identificador de tipo y type es para comprobar
En el parser cree
- El tipo command lo he cambiado bastante, ahora puede tener EvalOfTerm, EvalOfType, BindOfTerm o BindOfType
- Cree una funcion convert_type ctx ty que convierte un tipo TyDeclared en el tipo basico del que deriva
- Cambia un poco en typeof el de TmAbs

Cambios en lambda.mli
- Añadir los cambios que hice en el .ml

Cambios en el lexer.mll
- Añadidos los tokens IDT y TYPE

Cambios en el parser.mly
- Añadir esos nuevos tokens
- Añadidas nuevas reglas en s para poder añadir al contexto los tipos y funciones
- Añadida una regla en term para convertir el tipo de IDT

```
>> x = 10;;
- val : x : Nat = 10
>> N = Nat;;	/*esto seguiria la forma IDT EQ ty EOF en el parser*/
- : N = Nat
>> type N;;
- : type = Nat  /*usando type te dice de que tipo es el tipo creado*/
```




3. __Strings__
We added support for the string type.
To make this posible we created a new type, TyString, and a new term, TmString, in both lambda.ml and lambda.mli and modified the functions eval1, isval, subst, free_vars, string of term, typeof and string_of_ty so as to match this new changes added.
We also had to make modifications in the lexer.mll, where we added a new rule to match the structure of a string. By this rule, strings have to be encapsulated between quotation marks and must not contain neither the new line character (\n) nor quotation marks in between the ones that form the string in itself.
Finally, we had to expand the existing rules in the parser.mly file by adding the string type and term to both atomicTerm and atomicTy
We also implemented the functions concat (concatenates two diferent strings), first (returns the first character of a string as a string, or "" if the string is empty) and rest (returns a substring of the given string without the first element) for which we had to once again modify the formerly cited files, functions and rules (excluding the lexer.mll file)
```
>> x = "abcde";;
val x : String = "abcde"
>> concat x "fghij";;
- : String = "abcdefghij"
>> first x;;
- : String = "a"
>> rest x;;
- : String = "bcde"
>> first "";;
- : String = ""
>> rest "";;
- : String = ""
```