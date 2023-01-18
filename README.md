# larynx
## a human readable scripting language for beginners, written in rust


### About
larynx is very much in development. it should not be expected to always run properly or have any kind of stable API. when it is complete, larynx will be a tool for those who want to learn to program but find themselves intimidated by symbols and the like.

### To read these docs:

when representing how something is parsed, anything in square brackets [] is required, anything in curly braces {} is optional, and anything written literally is a hardcoded token.

these docs assume that you have a background in programming. later, (as larynx is intended for beginners), new docs will be written that explain basic programming concepts.

## Docs

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
currently, the only loops in larynx are while loops and for loops. while loops are declared as follows:

```
while [EXPRESSION] do
    [BLOCK]
end
```

for loops are a bit more complicated in syntax:
```
for every [IDENTIFIER] from [START] to [END] {stepping by [EXPRESSION]} do
    [BLOCK]
end
```
if the step is not specified, it is assumed to be 1. if START > END, then the loop will iterate backwards. for loops are inclusive to END.
