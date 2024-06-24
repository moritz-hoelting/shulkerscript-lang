# Grammar of the ShulkerScript language

## Table of contents

### Program
```ebnf
Program: Namespace Declaration*;
```

### Namespace
```ebnf
Namespace: 'namespace' StringLiteral;
```

### Declaration
```ebnf
Declaration: FunctionDeclaration | Import;
```

### Import
```ebnf
Import: 'from' StringLiteral 'import' Identifier;
```

### FunctionDeclaration
```ebnf
Function:
    Annotation* 'pub'? 'fn' Identifier '(' ParameterList? ')' Block
    ;
ParameterList:
    Identifier (',' Identifier)* ','?  
    ;
```

### Annotation
```ebnf
Annotation: '#[' Identifier ('=' StringLiteral)? ']';
```

### Statement
```ebnf
Statement:
    Block
    | LiteralCommand
    | Conditional
    | Grouping
    | DocComment
    | Semicolon
    | Run
    ;
```

### Block
```ebnf	
Block: '{' Statement* '}';
```

### Run
```ebnf
Run:
    'run' Expression ';'
    ;
```

### Conditional
```ebnf
Conditional:
    'if' ParenthizedCondition Block ('else' Block)?
    ;
```

### Condition
```ebnf
Condition:
    PrimaryCondition
    BinaryCondition
    ;
```

#### PrimaryCondition
```ebnf
PrimaryCondition:
    ConditionalPrefix
    | ParenthesizedCondition
    | StringLiteral
    ;
```

#### ConditionalPrefix
```ebnf
ConditionalPrefix:
    ConditionalPrefixOperator PrimaryCondition
    ;
```

#### ConditionalPrefixOperator
``` ebnf
ConditionalPrefixOperator: '!';
```

#### BinaryCondition
``` ebnf
BinaryCondition:
    Condition ConditionalBinaryOperator Condition
    ;
```

#### ConditionalBinaryOperator
``` ebnf
ConditionalBinaryOperator:
    '&&'
    | '||'
    ;
```

#### ParenthizedCondition
```ebnf
ParenthizedCondition:
    '(' Condition ')'
    ;
```


### Grouping
``` ebnf
Grouping:
    'group' Block
    ;
```

### Expression
```ebnf
Expression:
    Primary
    ;
```

### Primary
```ebnf
Primary:
    FunctionCall
    ;
```

### FunctionCall
```ebnf
FunctionCall:
    Identifier '(' (Expression (',' Expression)*)? ')'
    ;
```

### LuaCode
```ebnf
LuaCode:
    'lua' '(' (Expression (',' Expression)*)? ')' '{' (.*?)* '}'
    ;
```