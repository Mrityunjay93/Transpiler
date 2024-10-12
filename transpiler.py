import ply.lex as lex
import ply.yacc as yacc

# Lexer
tokens = (
    'INCLUDE', 'IOSTREAM', 'STRING_LITERAL',
    'INT', 'FLOAT', 'STRING', 'VOID',
    'IDENTIFIER', 'NUMBER',
    'PLUS', 'MINUS', 'TIMES', 'DIVIDE',
    'LPAREN', 'RPAREN', 'LBRACE', 'RBRACE',
    'SEMICOLON', 'EQUALS', 'COMMA',
    'COUT', 'CIN', 'LEFTSHIFT', 'RIGHTSHIFT',
    'IF', 'ELSE', 'WHILE', 'FOR', 'RETURN',
    'LESS', 'GREATER', 'LESSEQUAL', 'GREATEREQUAL'
)

t_PLUS = r'\+'
t_MINUS = r'-'
t_TIMES = r'\*'
t_DIVIDE = r'/'
t_LPAREN = r'\('
t_RPAREN = r'\)'
t_LBRACE = r'\{'
t_RBRACE = r'\}'
t_SEMICOLON = r';'
t_EQUALS = r'='
t_COMMA = r','
t_LESS = r'<'
t_GREATER = r'>'
t_LESSEQUAL = r'<='
t_GREATEREQUAL = r'>='

def t_LEFTSHIFT(t):
    r'<<'
    return t

def t_RIGHTSHIFT(t):
    r'>>'
    return t

reserved = {
    'include': 'INCLUDE',
    'iostream': 'IOSTREAM',
    'int': 'INT',
    'float': 'FLOAT',
    'string': 'STRING',
    'void': 'VOID',
    'cout': 'COUT',
    'cin': 'CIN',
    'if': 'IF',
    'else': 'ELSE',
    'while': 'WHILE',
    'for': 'FOR',
    'return': 'RETURN'
}

def t_IDENTIFIER(t):
    r'[a-zA-Z_][a-zA-Z_0-9]*'
    t.type = reserved.get(t.value, 'IDENTIFIER')
    return t

def t_NUMBER(t):
    r'\d+(\.\d+)?'
    t.value = float(t.value) if '.' in t.value else int(t.value)
    return t

def t_STRING_LITERAL(t):
    r'"[^"]*"'
    t.value = t.value[1:-1]  # Remove the quotes
    return t

t_ignore = ' \t\n'

def t_error(t):
    print(f"Illegal character '{t.value[0]}' at line {t.lineno}")
    t.lexer.skip(1)

# Parser
def p_program(p):
    '''program : statements'''
    p[0] = p[1]

def p_statements(p):
    '''statements : statement
                  | statements statement'''
    if len(p) == 2:
        p[0] = p[1]
    else:
        p[0] = p[1] + p[2]

def p_statement(p):
    '''statement : include_statement
                 | declaration
                 | assignment
                 | cout_statement
                 | cin_statement
                 | if_statement
                 | while_statement
                 | for_statement
                 | function_definition
                 | function_call
                 | return_statement
                 | expression SEMICOLON'''
    p[0] = p[1]

def p_include_statement(p):
    'include_statement : INCLUDE LESS IOSTREAM GREATER'
    p[0] = "# C++ iostream included\n"

def p_declaration(p):
    '''declaration : type IDENTIFIER SEMICOLON
                   | type IDENTIFIER EQUALS expression SEMICOLON'''
    if len(p) == 4:
        p[0] = f"{p[2]} = None  # {p[1]} in C++\n"
    else:
        p[0] = f"{p[2]} = {p[4]}  # {p[1]} in C++\n"

def p_type(p):
    '''type : INT
            | FLOAT
            | STRING
            | VOID'''
    p[0] = p[1]

def p_assignment(p):
    'assignment : IDENTIFIER EQUALS expression SEMICOLON'
    p[0] = f"{p[1]} = {p[3]}\n"

def p_cout_statement(p):
    'cout_statement : COUT LEFTSHIFT expression SEMICOLON'
    p[0] = f"print({p[3]})\n"

def p_cin_statement(p):
    'cin_statement : CIN RIGHTSHIFT IDENTIFIER SEMICOLON'
    p[0] = f"{p[3]} = input()\n"

def p_if_statement(p):
    '''if_statement : IF LPAREN expression RPAREN compound_statement
                    | IF LPAREN expression RPAREN compound_statement ELSE compound_statement'''
    if len(p) == 6:
        p[0] = f"if {p[3]}:\n{p[5]}"
    else:
        p[0] = f"if {p[3]}:\n{p[5]}else:\n{p[7]}"

def p_while_statement(p):
    'while_statement : WHILE LPAREN expression RPAREN compound_statement'
    p[0] = f"while {p[3]}:\n{p[5]}"

def p_for_statement(p):
    'for_statement : FOR LPAREN expression SEMICOLON expression SEMICOLON expression RPAREN compound_statement'
    p[0] = f"{p[3]}\nwhile {p[5]}:\n{p[9]}    {p[7]}\n"

def p_compound_statement(p):
    'compound_statement : LBRACE statements RBRACE'
    p[0] = '    ' + p[2].replace('\n', '\n    ').rstrip() + '\n'

def p_function_definition(p):
    'function_definition : type IDENTIFIER LPAREN parameters RPAREN compound_statement'
    p[0] = f"def {p[2]}({p[4]}):\n{p[6]}"

def p_parameters(p):
    '''parameters : type IDENTIFIER
                  | type IDENTIFIER COMMA parameters
                  | '''
    if len(p) == 1:
        p[0] = ""
    elif len(p) == 3:
        p[0] = p[2]
    else:
        p[0] = f"{p[2]}, {p[4]}"

def p_function_call(p):
    'function_call : IDENTIFIER LPAREN arguments RPAREN SEMICOLON'
    p[0] = f"{p[1]}({p[3]})\n"

def p_arguments(p):
    '''arguments : expression
                 | expression COMMA arguments
                 | '''
    if len(p) == 1:
        p[0] = ""
    elif len(p) == 2:
        p[0] = p[1]
    else:
        p[0] = f"{p[1]}, {p[3]}"

def p_return_statement(p):
    'return_statement : RETURN expression SEMICOLON'
    p[0] = f"return {p[2]}\n"

def p_expression(p):
    '''expression : IDENTIFIER
                  | NUMBER
                  | STRING_LITERAL
                  | expression PLUS expression
                  | expression MINUS expression
                  | expression TIMES expression
                  | expression DIVIDE expression
                  | expression LESS expression
                  | expression GREATER expression
                  | expression LESSEQUAL expression
                  | expression GREATEREQUAL expression
                  | LPAREN expression RPAREN'''
    if len(p) == 2:
        p[0] = p[1]
    elif len(p) == 4:
        if p[1] == '(':
            p[0] = f"({p[2]})"
        else:
            p[0] = f"{p[1]} {p[2]} {p[3]}"

def p_error(p):
    if p:
        print(f"Syntax error at '{p.value}' (line {p.lineno}, position {p.lexpos})")
    else:
        print("Syntax error at EOF")

# Build the lexer and parser
lexer = lex.lex()
parser = yacc.yacc()

# Read input file
with open('input.txt', 'r') as file:
    cpp_code = file.read()

# Parse and generate Python code
python_code = parser.parse(cpp_code, lexer=lexer)

# Write to output file
with open('output.txt', 'w') as file:
    if python_code is not None:
        file.write(python_code)
    else:
        file.write("# Unable to convert C++ code. Please check your input and error messages.\n")

print("Conversion completed. Check output.txt for the result.")