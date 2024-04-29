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
En el lexer está IDT que es el identificador de tipo
En el parser cree
- El tipo command lo he cambiado bastante, ahora puede tener EvalOfTerm, EvalOfType, BindOfTerm o BindOfType
- Cree una funcion convert_type ctx ty que convierte un tipo TyDeclared en el tipo basico del que deriva
- Cambia un poco en typeof el de TmAbs

Cambios en lambda.mli
- Añadir los cambios que hice en el .ml

Cambios en el lexer.mll
- Añadidos los tokens IDT

Cambios en el parser.mly
- Añadir esos nuevos tokens
- Añadidas nuevas reglas en s para poder añadir al contexto los tipos y funciones
- Añadida una regla en term para convertir el tipo de IDT

```
>> x = 10;;
- val : x : Nat = 10
>> N = Nat;;	/*esto seguiria la forma IDT EQ ty EOF en el parser*/
- : N = Nat
>> N;;
- : type = Nat  /*te dice de que tipo es el tipo creado*/
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

8. __Lists__
We implemented lists.
First we added in both lambda.ml and lambda.mli the type TyList and the terms TmList, TmEmptyList, TmIsEmptyList, TmHead and TmTail.
Then, in the lambda.ml file, we had to add the evaluation and typing rules for the lists in both the eval and typeof functions, and also modify some other functions such as string_of_term, string_of_ty, free_vars, subst and isval
We added new reserved tokens for the implementation of lists, which are "isEmptyList", "head", "hd", "tail", "tl" and ",".
And finally we had to modify the parser to add the new rules for the lists.
The rules in the parser for creating a list use recursivity, that is to say, when creating a list, you declare the first element, then declare a new list with the next element as its first element, then declare another list... and so on. Thus, for each list used you have to declare its type. Also, the last element of a list has to be an empty list.
We also implemented the functions isEmptyList, head and tail to work with functions, all of which receive as parameters a list and its type
```
>> list = [1, [2, [3, [4, [] : Nat] : Nat] : Nat] : Nat] : Nat;;
- : val list : Nat list = [1, 2, 3, 4]
>> list2 = [true, [true, [false, [] : Bool] : Bool] : Bool] : Bool;;
- : val list2 : Bool list = [true, true, false]
>> list3 = ["hola", ["mundo", [] : String] : String] : String;;
- : val list3 : String list = ["hola", "mundo"]
>> isEmptyList list : Nat;;
- : Bool = false
>> isEmptyList [] : Nat : Nat;;
- : Bool = true
>> head list : Nat;;
- : Nat = 1
>> tail list : Nat;;
- : Nat list = [2, 3, 4]
>> hd list : Nat;;
- : Nat = 1
>> tl list : Nat;;
- : Nat list = [2, 3, 4]
>> tl list2 : Bool;;
- : Bool list2 = [true, false]
>> hd [1, [2, [] : Nat] : Nat] : Nat : Nat;;
- : Nat = 1
```
You can create new functions making use of this implementation. For example
A function to calculate the length of a list
```
letrec length : [Nat] -> Nat =
	lambda l : [Nat].
		if (isEmptyList l : Nat) then 0
		else succ (length (tl l : Nat));;
```
A function to append two lists
```
letrec append : [Nat] -> [Nat] -> [Nat] = 
	lambda l1 : [Nat]. lambda l2 : [Nat].
		if (isEmptyList l1 : Nat) then l2
		else [(hd l1 : Nat), append (tl l1 : Nat) l2] : Nat;;
```
A map function
```
letrec map : [Nat] -> (Nat -> Nat) -> [Nat] = 
	lambda l : [Nat]. lambda f : (Nat -> Nat).
		if (isEmptyList l : Nat) then ([] : Nat)
		else [(f (hd l : Nat)), map (tl l : Nat) f] : Nat;;
```
*Note that these functions are implemented only to be used on lists of Nat*

9. __Subtyping__
We implemented subtyping.
By creating a function subtypeof, that makes use of the convert_type function, we made the language able to work with terms with different types as long as one is a subtype of the other.
This new function subtypeof is used in the typeof function in cases such as TmApp, TmFix and the diferent typing rules for Strings and Lists
```
>> N = Nat;;
- : N = Nat
>> N2 = Nat;;
- : N2 = Nat
>> l = [1, [2, [] : N2] : N2] : N2;;  
- : val l : N2 list = [1, 2]
>> l2 = [1, [2, [3, [] : N] : Nat] : N] : Nat;;
- : val l2 : Nat list = [1, 2, 3]

/* using the exact same implementation of the append fucntion previously defined */

>> append l l2;;
- : Nat list = [1, 2, 1, 2, 3]
```
