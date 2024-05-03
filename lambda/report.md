# Lambda Calculus Interpreter
## Programming Languages Design

### 1.- Improved functionalities
#### 1.1 __Multi-Line Expressions__
In order for this to work we are going to use the double semi-colon ';;' to marck the end of an input.

So we made two functions, “check_exp l p”  this function checks if the expression is complete by matching the list of strings, and “get_exp s”, this one is responsible for retrieving the complete expression from the user input. It uses the check_exp helper function to check if the expression is complete (i.e., ends with a double semicolon ;;). 

If the expression is not complete, it appends the next line of input (obtained by calling read_line()) to the original expression and recursively calls get_exp with the updated string, this allows the user to enter an expression that spans multiple lines, and the interpreter will continue reading until a complete expression is provided.


#### 1.2 __Pretty printer__
To improve readability, we add a pretty printer making use of the Format module.
We created 2 new files (pp.ml and pp.mli) in which we implement pp_type, pp_term_eval and pp_term_bind. This new functions, using Format's module's boxes, make the outputs of the language more human-readable
```
>> letrec length : [Nat] -> Nat =
	lambda l : [Nat].
		if (isEmptyList l : Nat) then 0
		else succ (length (tl l : Nat));;
 - : val 'length' : 'Nat' list -> Nat = 
λl : 'Nat' list. 
 if isemptylist  l  : Nat then 0 
  else succ fix λlength : 'Nat' list -> Nat. 
                 λl : 'Nat' list. 
                  if isemptylist  l  : Nat then 0 
                   else succ length (tail [l])   (tail [l]) 
```

### 2.- Added Functionalities
#### 2.1 __Internal Fixed Point Combinator__
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


#### 2.2 __Global Definitions Context__
We added the ability to associate free variables with values or terms.
This has been achieved by adding a new type, context "(string*binding) list" 
We also have a few functions to manage the context like, addbinding or getbinding_term, getbinding_type... The use of the addbinding and addbinding_type functions allows us to update the context as we evaluate expressions, while the getbinding_type and getbinding_term functions allows us to retrieve the necessary information from the context during the evaluation process.
The syntax to add new variables would be
```
>> x = 1 ;;
x : Nat = 1 

>> N = Nat ;;
N : type = Nat
```

#### 2.3 __Strings__
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

#### 2.4 __Tuples__
We implemented tuples. 
First we added in both lambda.ml and lambda.mli the type TyTuple and the term TmTuple.

Then, in lambda.ml we have to fill some functions to contemplate the Tuples, like typeof, string_of_term, string_of_ty, convert_type, free_vars, isval, subst, eval1
We also added the first and rest operations to Tuples

In order for this to work, we had to modify the parser.mly file by adding some new rules on how to construct a tuple (using "{" to open, and "}" to close it).
Some of this rules use recursivity, so we can make a list out of it.

The usage would be:

```
>> {1,2,3};;
- : {Nat, Nat, Nat} ={1, 2, 3} 
```

#### 2.5 __Records__
We implemented registers in a very similar way as tuples, but instead of a list of terms, in the case of TmRecord and TyRecord we use a list of (string * term) and a list of (string * type) respectively.
This is done so that we can store the tags of each element. Similarly to the tuples, we added the first and rest operations, and we needed a new case on the parser.

The usage would be:

```
>> {x=1, y=2, z=3};;
- : {x=Nat, y=Nat, z=Nat} = {x=1, y=2, z=3}
```


#### 2.7 __Variants__
Continuing with what was done previously, we added the variants in a similar way to records.
We create a new type TyVariant (string * ty) list, and for the labels we created TmLabel (strin * term * string).
A new case was required in parser to be added, as well as a few lexical rules.

As requested we have implemented the functionality ABS over the values of type Int made with variants.

The usage would be:
```
>> Int = <pos:Nat, zero:Bool, neg:Nat>;;
Int : type = <pos : Nat, zero : Bool, neg : Nat>

>> p3 = <pos=3> as Int;;
p3 : <pos : Nat, zero : Bool, neg : Nat> = <pos = 3>
```


#### 2.8 __Lists__
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

#### 2.9 __Subtyping__
We implemented subtyping.
By creating a function subtypeof, that makes use of the convert_type function, we made the language able to work with terms with different types as long as one is a subtype of the other.
This new function subtypeof is used in the typeof function in cases such as TmApp, TmFix and the diferent typing rules for Strings and Lists.
For tuples and records, subtyping is already implemented in them as they can have fields of different types 
```
>> N = Nat;;
- : N = Nat
>> N2 = Nat;;
- : N2 = Nat
>> l = [1, [2, [] : N2] : N2] : N2;;  
- : val l : N2 list = [1, 2]
>> l2 = [1, [2, [3, [] : N] : Nat] : N] : Nat;;
- : val l2 : Nat list = [1, 2, 3]

/* using the exact same implementation of the append function previously defined */

>> append l l2;;
- : Nat list = [1, 2, 1, 2, 3]
```
Applying subtyping to records
```
>> let id = lambda r : {}. r;;
 - : val 'id' : Record {} -> Record {} =  λr : Record {}.  r
>> id {x=1, y=2, z=3};;
 - : val =  Record {x = 1,  y = 2, z = 3}
>> x = {x=1, y=2};;
 - : val 'x' : Record {Nat, Nat} =  Record {x = 1,  y = 2}
>> id x;;
 - : val = Record {x = 1, y = 2}
```



