import ply.lex as lex
import ply.yacc as yacc

# --------------------- LEXER ---------------------
tokens = (
    'INCLUDE', 'IOSTREAM', 'STRING_LITERAL',
    'INT', 'FLOAT', 'STRING', 'VOID',
    'IDENTIFIER', 'NUMBER',
    'PLUS', 'MINUS', 'TIMES', 'DIVIDE',
    'LPAREN', 'RPAREN', 'LBRACE', 'RBRACE',
    'SEMICOLON', 'EQUALS', 'COMMA',
    'COUT', 'CIN', 'LEFTSHIFT', 'RIGHTSHIFT',
    'IF', 'ELSE', 'WHILE', 'FOR', 'RETURN',
    'LESS', 'GREATER', 'LESSEQUAL', 'GREATEREQUAL',
    'USING', 'NAMESPACE', 'STD'
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
    'return': 'RETURN',
    'using': 'USING',
    'namespace': 'NAMESPACE',
    'std': 'STD'
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
    t.value = '"' + t.value[1:-1] + '"'
    return t

t_ignore = ' \t\r'

def t_newline(t):
    r'\n+'
    t.lexer.lineno += len(t.value)

def t_error(t):
    print(f"Illegal character '{t.value[0]}' at line {t.lineno}")
    t.lexer.skip(1)

# --------------------- PARSER ---------------------
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
                 | using_namespace
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

def p_using_namespace(p):
    'using_namespace : USING NAMESPACE STD SEMICOLON'
    p[0] = "# using namespace std; (ignored in Python)\n"

def p_declaration(p):
    '''declaration : type declarator_list SEMICOLON'''
    decls = []
    for name, value in p[2]:
        if value is None:
            decls.append(f"{name} = None  # {p[1]} in C++")
        else:
            decls.append(f"{name} = {value}  # {p[1]} in C++")
    p[0] = '\n'.join(decls) + '\n'

def p_declarator_list_single(p):
    '''declarator_list : declarator'''
    p[0] = [p[1]]

def p_declarator_list_multi(p):
    '''declarator_list : declarator COMMA declarator_list'''
    p[0] = [p[1]] + p[3]

def p_declarator(p):
    '''declarator : IDENTIFIER
                  | IDENTIFIER EQUALS expression'''
    if len(p) == 2:
        p[0] = (p[1], None)
    else:
        p[0] = (p[1], p[3])

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
    '''cout_statement : COUT LEFTSHIFT cout_args SEMICOLON'''
    p[0] = f"print({p[3]})\n"

def p_cout_args(p):
    '''cout_args : expression
                 | cout_args LEFTSHIFT expression'''
    if len(p) == 2:
        p[0] = f"{p[1]}"
    else:
        p[0] = f"{p[1]}, {p[3]}"

def p_cin_statement(p):
    '''cin_statement : CIN RIGHTSHIFT cin_args SEMICOLON'''
    p[0] = p[3]

def p_cin_args_single(p):
    '''cin_args : IDENTIFIER'''
    p[0] = f"{p[1]} = input()\n"

def p_cin_args_multi(p):
    '''cin_args : IDENTIFIER RIGHTSHIFT cin_args'''
    p[0] = f"{p[1]} = input()\n" + p[3]

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
    '''for_statement : FOR LPAREN expression SEMICOLON expression SEMICOLON expression RPAREN compound_statement
                     | FOR LPAREN declaration expression SEMICOLON expression RPAREN compound_statement'''
    if len(p) == 10:
        # for (expr; expr; expr)
        p[0] = f"{p[3]}\nwhile {p[5]}:\n{indent(p[9])}{indent(p[7])}\n"
    else:
        # for (declaration; expr; expr)
        # p[3]: declaration, p[4]: cond, p[6]: inc, p[8]: body
        p[0] = f"{p[3]}while {p[4]}:\n{indent(p[8])}{indent(p[6])}\n"

def p_compound_statement(p):
    'compound_statement : LBRACE statements RBRACE'
    lines = p[2].split('\n')
    indented = '\n'.join('    ' + line if line.strip() else '' for line in lines)
    p[0] = indented.rstrip() + '\n'

def p_function_definition(p):
    '''function_definition : type IDENTIFIER LPAREN parameters RPAREN compound_statement'''
    if p[2] == "main":
        p[0] = f'if __name__ == "__main__":\n{p[6]}'
    else:
        p[0] = f"def {p[2]}({p[4]}):\n{p[6]}"

def p_parameters_empty(p):
    '''parameters : '''
    p[0] = ""

def p_parameters_single(p):
    '''parameters : type IDENTIFIER'''
    p[0] = p[2]

def p_parameters_multi(p):
    '''parameters : type IDENTIFIER COMMA parameters'''
    p[0] = f"{p[2]}, {p[4]}"

def p_function_call(p):
    'function_call : IDENTIFIER LPAREN arguments RPAREN SEMICOLON'
    p[0] = f"{p[1]}({p[3]})\n"

def p_arguments_empty(p):
    '''arguments : '''
    p[0] = ""

def p_arguments_single(p):
    '''arguments : expression'''
    p[0] = str(p[1])

def p_arguments_multi(p):
    '''arguments : expression COMMA arguments'''
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
        p[0] = str(p[1])
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

def indent(code):
    lines = code.rstrip('\n').split('\n')
    return '\n'.join('    ' + line if line.strip() else '' for line in lines) + '\n'

# --------------------- CONVERSION FUNCTION ---------------------
def convert_cpp_to_python(cpp_code):
    lexer = lex.lex()
    parser = yacc.yacc()
    try:
        result = parser.parse(cpp_code, lexer=lexer)
        return result if result else "# Unable to convert C++ code."
    except Exception as e:
        return f"# Error during parsing: {str(e)}"