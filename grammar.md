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
    ;
```

### Block
```ebnf	
Block: '{' Statement* '}';
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