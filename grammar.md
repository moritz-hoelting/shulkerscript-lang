# Grammar of the shulkerscript language

## Table of contents

### Program
```ebnf
Program: Declaration*;
```

### Declaration
```ebnf
Declaration: FunctionDeclaration;
```

### FunctionDeclaration
```ebnf
Function:
    Annotation* 'fn' Identifier '(' ParameterList? ')' Block
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

### ParenthizedCondition
```ebnf
ParenthizedCondition:
    '(' Condition ')'
    ;
```

### Condition
```ebnf
Condition:
    StringLiteral
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
```

### Primary
``` ebnf
Primary:
    FunctionCall
```

### FunctionCall
``` ebnf
FunctionCall:
    Identifier '(' (Expression (',' Expression)*)? ')'
    ;
```