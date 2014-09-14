Ftr terms

regles de precedence vont regler les problemes tout seul avec 

Conj -> pareil

Void instead of void


Notes on Ocaml parser for CS4212

The rules of the ocamlyacc parser generator follows mainly the rules of the grammar in BNF notation.

There is only some conflicts we had to fix:

### Reduce/shift conflict on VarDecl and MdDecl.
When begining a declaration in a class, we do not know if it s a variable or a method declaration.

With only:
```ocaml

classbody: { ([], []) }
        |   vardeclist mddecllist { ($1, $2) }
;

vardeclist : {[]} 
        | vardecl { $1 :: [] }
        | vardeclist vardecl { $2 :: $1 }
;
```
we do not know at the end of a variable declaration, if we should reduce the vardeclist, or shift to the methode declarations, due to the empty rule of vardeclist.


With:
```ocaml

classbody: { ([], []) }
        |   mddecllist { ([], $1) }
        |   vardeclist mddecllist { ($1, $2) }
;

vardeclist : vardecl { $1 :: [] }
        | vardeclist vardecl { $2 :: $1 }
;
```
We cannot have an empty vardeclist, which fixes the issue.

### Conj rule

we have two rules significating that that the AND_OPERATOR && have precedence over the OR_OPERATOR ||.
We can use only one rule usig the precedence declaration of token in ocamlyacc:

```ocaml
%left OR_OPERATOR
%left AND_OPERATOR
```
That means that both operators are left associative, and that AND_OPERATOR have higher precedence over OR_OPERATOR.

### AExp rule

We have the same tip with the arithmetic operators +, - /, *.

```ocaml
%left PLUS MINUS
%left MULTIPLY DIVIDE
```
That means that the 4 operators are left associative, and that MULTIPLY and DIVIDE operators have higher precedence over PLUS and MINUS operators.

### List 

### Comments

We use another rule in the lexer in order not to read pass any unuseful content to the parser.
We have to use two rules, for each comment use. (On line, Mulitlines)

### AST Trees 

To build the tree, we had to pass the arguments from the inside terminals or non-terminals.
Depending on the jlite formula, we sometimes have to dispatch the arguments.
We created first_item and second_item procedures to fetch the first and second item of a list/tuple.