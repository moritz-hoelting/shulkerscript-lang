# Grammar of the Shulkerscript language

## Program

```ebnf
Program:
  Namespace
  Declaration*
  ;
```

## Declaration

```ebnf
Declaration:
    Function
    | Import
    | TagDeclaration
    | ('pub'? VariableDeclaration ';')
  ;
```

## Namespace

```ebnf
Namespace:
   'namespace' StringLiteral ';' ;
```

## Function

```ebnf
Function:
    Annotation* 'pub'? 'fn' Identifier '(' FunctionParameterList? ')' Block
    ;

```

## Import

```ebnf
Import:
    'from' StringLiteral 'import' ('*' | Identifier (',' Identifier)*) ';'
    ;
```

## TagDeclaration

```ebnf
TagDeclaration:
    'tag' ('<' StringLiteral '>')? StringLiteral 'replace'? '[' (StringLiteral (',' StringLiteral)*)? ']'
    ;
```

## VariableDeclaration

```ebnf
VariableDeclaration:
   SingleVariableDeclaration
   | ArrayVariableDeclaration
   | ScoreVariableDeclaration
   | TagVariableDeclaration
   | ComptimeValueDeclaration
   ;
```

## StringLiteral

```ebnf
StringLiteral:
  '"' TEXT '"';
```

## Annotation

```ebnf
Annotation:
    '#[' AnnotationAssignment ']'
    ;
```

## Block

```ebnf
Block:
    '{' Statement* '}'
    ;
```

## FunctionParameterList

```ebnf
FunctionParameterList:
    FunctionArgument (',' FunctionArgument)* ','?  
    ;
```

## ArrayVariableDeclaration

```ebnf
ArrayVariableDeclaration:
    ('int' | 'bool') '[' integer ']' identifier VariableDeclarationAssignment?
```

## ComptimeValueDeclaration

```ebnf
ComptimeValueDeclaration:
    'val' identifier VariableDeclarationAssignment?
```

## ScoreVariableDeclaration

```ebnf
ScoreVariableDeclaration:
    'int' ('<' StringLiteral '>')? identifier '[' AnyStringLiteral? ']' VariableDeclarationAssignment?
```

## SingleVariableDeclaration

```ebnf
SingleVariableDeclaration:
    ('int' | 'bool') identifier VariableDeclarationAssignment?
```

## TagVariableDeclaration

```ebnf
TagVariableDeclaration:
    'bool' identifier '[' AnyStringLiteral? ']' VariableDeclarationAssignment?
```

## AnnotationAssignment

```ebnf
AnnotationAssignment:
    Identifier AnnotationValue
    ;
```

## Statement

```ebnf
Statement:
    Block
    | LiteralCommand
    | Conditional
    | Grouping
    | DocComment
    | ExecuteBlock
    | Semicolon
    ;
```

## FunctionArgument

```ebnf
FunctionArgument:
    FunctionVariableType Identifier
    ;
```

## VariableDeclarationAssignment

```ebnf
VariableDeclarationAssignment:
    '=' Expression
```

## AnyStringLiteral

```ebnf
AnyStringLiteral: StringLiteral | MacroStringLiteral ;
```

## AnnotationValue

```ebnf
AnnotationValue:
    '=' Expression
    | '(' AnnotationAssignment ( ',' AnnotationAssignment )* ')'
    ;
```

## Conditional

```ebnf
Conditional:
'if' Parenthized
;
```

## ExecuteBlock

```ebnf
ExecuteBlock:
   (ExecuteBlockHead ExecuteBlockTail)
  | (Conditional Block Else)
  ;
```

## Grouping

```ebnf
Grouping:
  'group' Block
;
```

## Semicolon

```ebnf
Semicolon:
  SemicolonStatement ';'
  ;
```

## FunctionVariableType

```ebnf
FunctionVariableType:
    'macro' | 'int' | 'bool'
    ;
```

## Expression

```ebnf
Expression:
    Primary | Binary ;
```

## MacroStringLiteral

```ebnf
MacroStringLiteral:
  '`' ( TEXT | '$(' [a-zA-Z0-9_]+ ')' )* '`';
```

## Else

```ebnf
Else:
    'else' Block
    ;
```

## ExecuteBlockHead

```ebnf
ExecuteBlockHead:
  Conditional
 | Align
 | Anchored
 | As
 | AsAt
 | At
 | Facing
 | In
 | On
 | Positioned
 | Rotated
 | Store
 | Summon
;
```

## ExecuteBlockTail

```ebnf
ExecuteBlockTail:
 ExecuteBlock
| Block
;
```

## SemicolonStatement

```ebnf
SemicolonStatement:
  (Expression | VariableDeclaration | Assignment | ReturnStatement)
  ';'
  ;
```

## Binary

```ebnf
Binary:
    Expression BinaryOperator Expression
    ;
```

## Primary

```ebnf
Primary:
    Identifier
    | Prefix
    | Parenthesized
    | Indexed
    | Integer
    | Boolean
    | StringLiteral
    | FunctionCall
    | MemberAccess
    | MacroStringLiteral
    | LuaCode
```

## Align

```ebnf
Align:
  'align' '(' AnyStringLiteral ')' ;
```

## Anchored

```ebnf
Anchored:
  'anchored' '(' AnyStringLiteral ')' ;
```

## As

```ebnf
As:
  'as' '(' AnyStringLiteral ')' ;
```

## AsAt

```ebnf
AsAt:
  'asat' '(' AnyStringLiteral ')' ;
```

## At

```ebnf
At:
  'at' '(' AnyStringLiteral ')' ;
```

## Facing

```ebnf
Facing:
  'facing' '(' AnyStringLiteral ')' ;
```

## In

```ebnf
In:
  'in' '(' AnyStringLiteral ')' ;
```

## On

```ebnf
On:
  'on' '(' AnyStringLiteral ')' ;
```

## Positioned

```ebnf
Positioned:
  'positioned' '(' AnyStringLiteral ')' ;
```

## Rotated

```ebnf
Rotated:
  'rotated' '(' AnyStringLiteral ')' ;
```

## Store

```ebnf
Store:
  'store' '(' AnyStringLiteral ')' ;
```

## Summon

```ebnf
Summon:
  'summon' '(' AnyStringLiteral ')' ;
```

## Assignment

```ebnf
Assignment:
   AssignmentDestination '=' Expression
```

## ReturnStatement

```ebnf
ReturnStatement:
    `return` Expression ;
```

## BinaryOperator

```ebnf
BinaryOperator:
    '+'
    | '-'
    | '*'
    | '/'
    | '%'
    | '=='
    | '!='
    | '<'
    | '<='
    | '>'
    | '>='
    | '&&'
    | '||'
    ;
```

## FunctionCall

```ebnf
FunctionCall:
    Identifier '(' (Expression (',' Expression)*)? ')'
    ;
```

## Indexed

```ebnf
Indexed:
    PrimaryExpression '[' Expression ']'
    ;
```

## LuaCode

```ebnf
LuaCode:
    'lua' '(' (Expression (',' Expression)*)? ')' '{' (.*?)* '}'
```

## MemberAccess

```ebnf
MemberAccess:
    Primary '.' Identifier
```

## Parenthesized

```ebnf
Parenthesized:
    '(' Expression ')'
    ;
```

## Prefix

```ebnf
Prefix:
    PrefixOperator Primary
    ;
```

## AssignmentDestination

```ebnf
AssignmentDestination:
    Identifier
    | Identifier '[' Expression ']'
    ;
```

## PrefixOperator

```ebnf
PrefixOperator:
    '!' | '-' | 'run'
    ;
```

