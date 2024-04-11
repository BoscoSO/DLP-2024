# Lambda Calculus Interpreter
## Programming Languages Design

### 1.- Improved functionalities

### 2.- Added Functionalities
1. __Internal Fixed Point Combinator__
To simplify the declaration of recursive functions, we added a new token to the syntax of the interpreter, 'LETREC'. This token now allows the interpreter to know that the function is recursive and process it as such.
For this, we added a new type of term, 'TmFix'
To prove the correctfullness of the behaviour of this new functionality, we provide, both here and in the examples.txt file, the following implementations of the product, Fibonacci and factorial functions:
```letrec sum: todo letrec prod: todo letrec fib: todo letrec fact: todo```