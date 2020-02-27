INTEGER, PLUS, MINUS, MUL, DIV, LPAR, RPAR, EOF = {
    'INT', 'PLUS', 'MINUS','MUL','DIV','(',')','EOF'
}

class Token():
    def __init__(self, type, val):
        self.type=type
        self.val=val

class Lexer():
    def __init__(self, text):
        self.text=text
        self.pos=0
        self.cur_char=self.text[self.pos]

    def error(self):
        raise Exception('invalid char')

    def advance(self):
       self.pos+=1
       if (self.pos < len(self.text)):
           self.cur_char=self.text[self.pos]
       else:
           self.cur_char=None

    def skip_whitespace(self):
        while self.cur_char is not None and self.cur_char.isspace():
            self.advance()

    def integer(self):
        ret = ''
        while self.cur_char is not None and self.cur_char.isdigit():
            ret+=self.cur_char
            self.advance()
        return int(ret)

    def get_next_token(self):
        while self.cur_char is not None:
            if self.cur_char.isspace():
                self.skip_whitespace()
                continue
            elif self.cur_char.isdigit():
                return Token(INTEGER,self.integer())
            elif self.cur_char == '+':
                self.advance()
                return Token(PLUS, '+')
            elif self.cur_char == '-':
                self.advance()
                return Token(MINUS, '-')
            elif self.cur_char == '*':
                self.advance()
                return Token(MUL, '*')
            elif self.cur_char == '/':
                self.advance()
                return Token(DIV, '/')
            elif self.cur_char == '(':
                self.advance()
                return Token(LPAR, '(')
            elif self.cur_char == ')':
                self.advance()
                return Token(RPAR, ')')
            else:
                self.error()
        return Token(EOF,None)

class Ast():
    pass

class BinOp(Ast):
    def __init__(self, left, op, right):
        self.left = left
        self.token=self.op=op
        self.right=right

class UnaryOp(Ast):
    def __init__(self, op, expr):
        self.op = self.token = op
        self.expr = expr

class Num(Ast):
    def __init__(self,token):
        self.token=token
        self.val=token.val

class Parser():
    def __init__(self,lexer):
        self.lexer=lexer
        self.cur_token = self.lexer.get_next_token()

    def error(self):
        raise  Exception('Invalid syntax')

    def eat(self, type):
        if self.cur_token.type == type:
            self.cur_token = self.lexer.get_next_token()
        else:
            self.error()

    def factor(self):
        """factor : (PLUS | MINUS) factor | INTEGER | LPAREN expr RPAREN"""
        cur_token = self.cur_token
        if cur_token.type == PLUS:
            self.eat(PLUS)
            return UnaryOp(cur_token,self.factor())
        elif cur_token.type == MINUS:
            self.eat(MINUS)
            return UnaryOp(cur_token, self.factor())
        elif cur_token.type == INTEGER:
            self.eat(INTEGER)
            return Num(cur_token)
        elif cur_token.type == LPAR:
            self.eat(LPAR)
            node = self.expr()
            self.eat(RPAR)
            return node


        self.error()

    def term(self):
        """term : factor ((MUL | DIV) factor)*"""
        node = self.factor()

        while self.cur_token.type in (MUL , DIV):
            cur_token = self.cur_token
            if cur_token.type == MUL:
                self.eat(MUL)
            elif cur_token.type == DIV:
                self.eat(DIV)
            node = BinOp(left=node, op=cur_token,right=self.factor())
        return node

    def expr(self):
        """
        	expr   : term ((PLUS | MINUS) term)*
        	term   : factor ((MUL | DIV) factor)*
        	factor : INTEGER | LPAREN expr RPAREN
        	"""
        node = self.term()
        while self.cur_token.type in (PLUS, MINUS):
            cur_token = self.cur_token
            if cur_token.type == PLUS:
                self.eat(PLUS)
            elif cur_token.type == MINUS:
                self.eat(MINUS)
            node = BinOp(left=node, op=cur_token, right=self.term())
        return node

    def parse(self):
         return self.expr()

class NodeVisiter():
    def visit(self, node):
        method_name = 'visit_' + type(node).__name__
        visitor = getattr(self,method_name,self.generic_visit)
        return visitor(node)
    def generic_visit(self,node):
        raise Exception('no visit_{} method type'.format(type(node).__name__))

class Intepreter(NodeVisiter):
    def __init__(self, parser):
        self.parser= parser

    def visit_BinOp(self,node):
        if node.op.type == PLUS:
            return self.visit(node.left) + self.visit(node.right)
        elif node.op.type == MINUS:
            return self.visit(node.left) - self.visit(node.right)
        elif node.op.type == MUL:
            return self.visit(node.left) * self.visit(node.right)
        elif node.op.type == DIV:
            return self.visit(node.left) / self.visit(node.right)

    def visit_Num(self,node):
        return node.val

    def visit_UnaryOp(self, node):
        if node.op.type == PLUS:
            return self.visit(node.expr)
        elif node.op.type == MINUS:
            return -self.visit(node.expr)

    def intepreter(self):
        node = self.parser.parse()
        return self.visit(node)

def main():
    while True:
	    text = input('spi> ')

	    if len(text.strip()):
	        parser = Parser(Lexer(text))
	        interpreter = Intepreter(parser)
	        result = interpreter.intepreter()
	        print(result)

if __name__ == '__main__':
    main()