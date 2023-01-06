# larynx
## a human readable scripting language for beginners, written in rust


### About
larynx is very much in development. it isn't even turing-complete right now, and should not be expected to always run properly or have any kind of stable API. when it is complete, larynx will be a tool for those who want to learn to program but find themselves intimidated by symbols and the like.

### Docs

#### Data types
larynx currently has the following data types:
* number, which stores all numbers
* text, which can store any text
* truth, which stores either true or false
* nothing, which acts as an empty unit type

#### Math
larynx currently supports the following mathematical operations with the following keywords:
* plus
* minus
* times
* divided by

#### Input/Output
larynx currently only has a simple output operator 'say' that outputs a single expression and a newline.

#### Variables
variables are declared and assigned in larynx with the 'is' keyword. the syntax is:
```
[IDENTIFIER] is [EXPRESSION]
```
filling in any variable name for \[IDENTIFIER] and any expression for \[EXPRESSION].
after this, the identifier will represent the expression in code.


variables can be deallocated and deleted with the 'delete' keyword. the syntax is:
```
delete [IDENTIFIER]
```


#### Logical operators
larynx supports the following logical operators for truth values:
* and
* or
* not

#### Equality
equality of any 2 expressions can be checked with the 'equals' keyword, which evaluates to a truth. 

#### Concatenation
if the 'plus' operator is used on two texts, they will be concatenated.

#### Control flow
if statements are done like this:
```
if [EXPRESSION] then
    [BLOCK]
end
```
where \[EXPRESSION] is any truth value and \[BLOCK] is any amount of larynx code.

else statements may be appended to an if statement after the `end` keyword. example:

```
if [EXPRESSION] then
    [BLOCK]
end
else
    [BLOCK]
end
```

#### Loops
currently, the only loops in larynx are while loops. while loops are declared as follows:

```
while [EXPRESSION] do
    [BLOCK]
end
```

