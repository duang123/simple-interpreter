
import collections
BEGIN, END, DOT, SEMI, PLUS, MINUS, MUL,  EOF, ID, ASSIGN, LPAR, RPAR, PROGRAM, VAR, COLON, COMMA, INTEGER, REAL, INT_CONST, REAL_CONST, INT_DIV, REAL_DIV= \
'BEGIN', 'END', '.', ';', '+', '-', '*', None, 'id', '=', '(', ')', 'prog', 'var', ':', ',','int','real','int_const', 'real_const','int_div', 'real_div'

class Token:
    def __init__(self, type, val):
        self.type = type
        self.val = val

RESERVE_KEYWORDS = {
    'BEGIN' : Token(BEGIN, 'begin'),
    'END': Token(END,'end'),
    'PROGRAM':Token(PROGRAM, 'prog'),
    'VAR' : Token(VAR, 'var'),
    'INTEGER':Token(INTEGER, 'int'),
    'REAL':Token(REAL,'real'),
    'DIV':Token(INT_DIV,'div'),
}

class Lexer:
    def __init__(self, text):
        self.text = text
        self.pos = 0
        self.cur_char = self.text[0]

    def error(self):
        raise Exception('invalid char')

    def advance(self):
        self.pos+=1
        if self.pos < len(self.text):
            self.cur_char = self.text[self.pos]
        else:
            self.cur_char = None
    def skip_comment(self):
        while self.cur_char != '}':
            self.advance()
        self.advance()

    def skip_space(self):
        while self.pos < len(self.text) and self.cur_char.isspace():
            self.advance()

    def peek(self):
        pos = self.pos+1
        if pos < len(self.text):
            return self.text[pos]
        else:
            return None

    def number(self):
        ret = ''
        while self.pos < len(self.text) and self.cur_char.isdigit():
            ret += self.cur_char
            self.advance()
        if self.cur_char == '.':
            ret += self.cur_char
            self.advance()
            while self.cur_char is not None and self.cur_char.isdigit():
                ret+=self.cur_char
                self.advance()
            return Token(REAL_CONST, float(ret))
        else: return Token(INT_CONST, int(ret))

    def id(self):
        ret = ''
        while self.pos < len(self.text) and self.cur_char.isalnum():
            ret += self.cur_char
            self.advance()
        return RESERVE_KEYWORDS.get(ret, Token(ID, ret))

    def get_next_token(self):
        while self.pos < len(self.text):
            if self.cur_char.isspace():
                self.skip_space()
                continue
            if self.cur_char == '{':
                self.advance()
                self.skip_comment()
                continue
            if self.cur_char.isdigit():
                return self.number()
            if self.cur_char.isalpha():
                return self.id()
            if self.cur_char == '+':
                self.advance()
                return Token(PLUS,'+')
            if self.cur_char == '-':
                self.advance()
                return Token(MINUS, '-')
            if self.cur_char == '*':
                self.advance()
                return Token(MUL, '*')
            if self.cur_char == '/':
                self.advance()
                return Token(REAL_DIV, '/')
            if self.cur_char == '.':
                self.advance()
                return Token(DOT,'.')
            if self.cur_char == ';':
                self.advance()
                return Token(SEMI,';')
            if self.cur_char == ':' and self.peek() == '=':
                self.advance()
                self.advance()
                return Token(ASSIGN, '=')
            if self.cur_char == ':':
                self.advance()
                return Token(COLON, ':')
            if self.cur_char == ',':
                self.advance()
                return Token(COMMA, ',')
            if self.cur_char == '(':
                self.advance()
                return Token(LPAR, '(')
            if self.cur_char == ')':
                self.advance()
                return Token(RPAR, ')')
            else:
                self.error()
        return Token(EOF, None)

class Ast():
    pass

class Program(Ast):
    def __init__(self, name, block):
        self.name = name
        self.block = block

class Block(Ast):
    def __init__(self, declaration, compound):
        self.declaration=declaration
        self.compound = compound

class VarDecl(Ast):
    def __init__(self, var_node, type_node):
        self.var_node = var_node
        self.type_node = type_node

class Type(Ast):
    def __init__(self, token):
        self.token = token
        self.val = token.val

class Compound(Ast):
      def __init__(self):
          self.children = []

class Assign(Ast):
    def __init__(self, left, op, right):
        self.left = left
        self.op = op
        self.right = right

class Var(Ast):
    def __init__(self, token):
        self.token = token
        self.val = self.token.val

class BinOp(Ast):
    def __init__(self, left, op, right):
        self.left = left
        self.op = self.token = op
        self.right = right

class UnaryOp(Ast):
    def __init__(self, token, expr):
        self.token = token
        self.expr = expr

class Num(Ast):
    def __init__(self, token):
        self.token = token
        self.val = token.val

class NoOp(Ast):
    pass

class Parser():
    def __init__(self, lexer):
        self.lexer = lexer
        self.cur_token = self.lexer.get_next_token()

    def error(self):
        raise Exception('error synax')

    def eat(self, type):
        if type == self.cur_token.type:
            self.cur_token = self.lexer.get_next_token()
        else:
            self.error()

    def id(self):
        op = self.cur_token
        if self.cur_token.type == ID:
            self.eat(ID)
            return Var(op)

    def factor(self):
        """" factor : PLUS factor
       | MINUS factor
       | INTEGER_CONST
       | REAL_CONST
       | LPAREN expr RPAREN
       | variable """
        op = self.cur_token
        if op.type == PLUS:
            self.eat(PLUS)
            node = self.factor()
            return UnaryOp(op, node)
        elif op.type == MINUS:
            self.eat(MINUS)
            node = self.factor()
            return UnaryOp(op, node)
        elif op.type in (REAL_CONST, INT_CONST):
            self.eat(op.type)
            return Num(op)
        elif op.type == LPAR:
            self.eat(LPAR)
            node = self.expr()
            self.eat(RPAR)
            return node
        elif op.type == ID:
            return self.id()

    def term(self):
        """" term : factor ((MUL | INTEGER_DIV | FLOAT_DIV) factor)*
 """
        node = self.factor()
        while self.cur_token.type in (MUL, INT_DIV, REAL_DIV ):
            op = self.cur_token
            if op.type == MUL:
                self.eat(MUL)
            elif op.type == INT_DIV:
                self.eat(INT_DIV)
            elif op.type == REAL_DIV:
                self.eat(REAL_DIV)
            right = self.factor()
            node = BinOp(node, op, right)
        return node

    def expr(self):
        """" expr : term ((PLUS | MINUS) term)* """
        node = self.term()
        while self.cur_token.type in (PLUS, MINUS):
            op = self.cur_token
            if op.type == PLUS:
                self.eat(PLUS)
            elif op.type == MINUS:
                self.eat(MINUS)
            node = BinOp(left=node, op=op, right=self.term())
        return node

    def variable(self):
        """" variable : ID """
        token = self.cur_token
        self.eat(ID)
        return Var(token)

    def assign_statement(self):
        """" assignment_statement : variable ASSIGN expr """

        left = self.variable()
        self.eat(ASSIGN)
        right = self.expr()
        return Assign(left, Token(ASSIGN, '='), right)


    def empty(self):
        return NoOp()

    def statement(self):
        """"statement : compound_statement
          | assignment_statement
          | empty """
        if self.cur_token.type == BEGIN:
            node = self.compound_statement()
        elif self.cur_token.type == ID:
            node = self.assign_statement()
        else:
            node = self.empty()
        return node

    def statement_list(self):
        """""  statement_list : statement
               | statement SEMI statement_list       """
        children = []
        node = self.statement()
        children.append(node)
        while self.cur_token.type == SEMI:
            self.eat(SEMI)
            children.append(self.statement())
        return children

    def compound_statement(self):
        """"compound_statement : BEGIN statement_list END"""
        self.eat(BEGIN)
        nodes = self.statement_list()
        self.eat(END)
        root = Compound()
        for ele in nodes:
            root.children.append(ele)
        return root

    def type_spec(self):
        """ type_spec : INTEGER
          | REAL """
        cur_token = self.cur_token
        if cur_token.type == INTEGER:
            self.eat(INTEGER)
        elif cur_token.type == REAL:
            self.eat(REAL)
        return Type(cur_token)
    def variable_declarations(self):
        """ variable_declaration : ID (COMMA ID)* COLON type_spec """
        var_list = [Var(self.cur_token)]
        self.eat(ID)
        while self.cur_token.type == COMMA:
            self.eat(COMMA)
            var_list.append(Var(self.cur_token))
            self.eat(ID)
        self.eat(COLON)

        """for child in  var_list:
            var_decs.append(VarDecl(child, type))"""
        type_node = self.type_spec()
        var_declarations = [
            VarDecl(var_node, type_node)
            for var_node in var_list
        ]
        return var_declarations

    def declarations(self):
        """ declarations : VAR (variable_declaration SEMI)+
             | empty """
        var_dec = []
        if self.cur_token.type == VAR:
            self.eat(VAR)

            while self.cur_token.type == ID:
                var_dec.extend(self.variable_declarations())
                self.eat(SEMI)
        return var_dec


    def block(self):
        """" block : declarations compound_statement """
        decl = self.declarations()
        compound = self.compound_statement()
        return Block(decl, compound)

    def program(self):
        """ program : program : PROGRAM variable SEMI block DOT
 """
        self.eat(PROGRAM)
        var = self.variable()
        self.eat(SEMI)
        block = self.block()
        self.eat(DOT)
        return Program(var.val, block)

    def parse(self):
        node = self.program()
        if self.cur_token.type != EOF:
            self.error()
        return node

class NodeVisitor():
    def visit(self, node):
        name = 'visit_' + type(node).__name__
        visitor=getattr(self, name, self.generic_visit)
        return visitor(node)

    def generic_visit(self, node):
        raise Exception('No visit_{} method'.format(type(node).__name__))

class Symbol():
    def __init__(self, name, type= None):
        self.name = name
        self.type = type

class VarSym(Symbol):
    def __init__(self, name, type):
        super(VarSym,self).__init__(name, type)

class BuiltinTypeSym(Symbol):
    def __init__(self, name):
        super(BuiltinTypeSym, self).__init__(name)

class SymTable():
    def __init__(self):
        self.symTab = collections.OrderedDict()

    def _init_builtins(self):
        self.define(BuiltinTypeSym('real'))
        self.define(BuiltinTypeSym('int'))

    def define(self, symbol):
        print( 'define %s' %symbol)
        self.symTab[symbol.name] = symbol

    def lookup(self, name):
        print('lookup %s' %name)
        symbol = self.symTab.get(name)
        return symbol

class SymTableBuilder(NodeVisitor):
    def __init__(self):
        self.symbols = SymTable()

    def visit_Program(self, node):
        self.visit(node.block)

    def visit_Block(self, node):
        for ele in node.declaration:
            self.visit(ele)
        self.visit(node.compound)

    def visit_VarDecl(self, node):
        """ var_node type_node"""
        name = node.type_node.val
        type = self.symbols.lookup(name)
        var_sym = VarSym(node.var_node.val, type)
        self.symbols.define(var_sym)

    def visit_Compound(self, node):
        for ele in node.children:
            self.visit(ele)

    def visit_Assign(self, node):
        var_name = self.symbols.lookup(node.left.val)
        if var_name is None:
            raise Exception('no such %s' %node.left.val)
        self.visit(node.right)

    def visit_Var(self, node):
        var_name = self.symbols.lookup(node.val)
        if var_name is None:
            raise Exception('no such %s' %node.left.val)

    def visit_BinOp(self, node):
        self.visit(node.left)
        self.visit(node.right)

    def visit_UnaryOp(self, node):
        self.visit(node.expr)

    def visit_Num(self, node):
        pass

    def visit_NoOp(self, node):
        pass

class Intepreter(NodeVisitor):
    def __init__(self, tree):
        self.tree = tree
    GLOBAL_SCOPE ={}
    def visit_BinOp(self, node):
        if (node.op.type == PLUS):
            return self.visit(node.left) + self.visit(node.right)
        elif node.op.type == MINUS:
            return self.visit(node.left) - self.visit(node.right)
        elif node.op.type == MUL:
            return self.visit(node.left) * self.visit(node.right)
        elif node.op.type == REAL_DIV:
            return float(self.visit(node.left)) / float(self.visit(node.right))
        elif node.op.type == INT_DIV:
            return self.visit(node.left) / self.visit(node.right)
    def visit_Num(self, node):
        return node.val

    def visit_UnaryOp(self, node):
        if (node.token.type == PLUS):
            return self.visit(node.expr)
        elif node.token.type == MINUS:
            return -self.visit(node.expr)

    def visit_NoOp(self, node):
        pass

    def visit_Compound(self, node):
        for ch in node.children:
            self.visit(ch)

    def visit_Assign(self, node):
        name = node.left.val
        self.GLOBAL_SCOPE[name] = self.visit(node.right)

    def visit_Var(self, node):
        var = self.GLOBAL_SCOPE.get(node.val)
        if  var is not None:
            return var
        else:
            raise Exception('no such var')
    def visit_VarDecl(self,node):
        pass

    def visit_Type(self,node):
        pass

    def visit_Block(self, node):
        for ele in node.declaration:
            self.visit(ele)
        self.visit(node.compound)

    def visit_Program(self, node):
        self.visit(node.block)

    def intepreter(self):
        tree = self.tree
        self.visit(tree)

def main():
    import sys
    text = open(sys.argv[1], 'r').read()

    lexer = Lexer(text)
    parser = Parser(lexer)
    tree = parser.parse()
    symtab_builder = SymTableBuilder()
    symtab_builder.visit(tree)
    print('')
    print('Symbol Table contents:')
    print(symtab_builder.symbols)

    interpreter = Intepreter(tree)
    result = interpreter.intepreter()

    print('')
    print('Run-time GLOBAL_MEMORY contents:')
    for k, v in sorted(interpreter.GLOBAL_SCOPE.items()):
        print('{} = {}'.format(k, v))

if __name__ == '__main__':
    main()
