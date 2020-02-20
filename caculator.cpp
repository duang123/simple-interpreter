#include <iostream>
#include <string>
using namespace std;

enum TokenType {
	INTEGER, PLUS, MINUS, MUL, DIV, LPAREN, RPAREN, END,
};
class Token{
public:
   Token():type(END),value(0){}
   Token(int type, int value):type(type),value(value){}

//private:
int type,value;
};

class Lexer{
public:
  Lexer(const string& text):text(text),pos(0),c(text[0]){} 
  void advance(){
      pos+=1;
     if(pos >= text.size()) 
	     c='\0';
     else c = text[pos];
  }
  void skipSpace(){
	  while(pos < text.size() && isspace(c)){
		  advance();
	  }
  }
  int interger(){
    string ret;
    while(pos < text.size() && isdigit(c)){
      ret.push_back(c);
      advance();
    }
   return stoi(ret);
  }
  Token getNextToken(){
   while(pos < text.size()){
     if(isspace(c)){
	skipSpace();
        continue;
     }else if (isdigit(c)){
       return Token(INTEGER, interger());
     }else if (c == '+'){
      advance();
      return Token(PLUS,'+');
     }else if(c == '-'){
       advance();
       return Token(MINUS, '-');
     }else if (c == '*'){
       advance();
       return Token(MUL, '*');
     }else if(c == '/'){
       advance();
       return Token(DIV,'/');
     }else if (c == '('){
       advance();
       return Token(LPAREN, '(');
     }else if (c == ')'){
       advance();
       return Token(RPAREN, ')');
     }

   }  
   return Token(END,'\0');
  }

private:
string text;
int pos;
char c;
};

class Calc{
public:
	Calc(Lexer& l):lexer(l){curToken = lexer.getNextToken();}
	/*
	 expr   : term ((PLUS | MINUS) term)*
	term   : factor ((MUL | DIV) factor)*
	factor : INTEGER | LPAREN expr RPAREN
	 */
        void eat(int type){
	  if (curToken.type == type){
	    curToken = lexer.getNextToken();
	  }
	}

	int factor(){
		int f;
	  if (curToken.type == INTEGER){
	       f = curToken.value; 
		  eat(INTEGER);
	  }
	  else if (curToken.type == LPAREN){
		  eat(LPAREN);
		  f = expr();
		    eat(RPAREN);
		  }
	  
          return f;
	
	}
	int term(){
	  int f = factor();
	  while (curToken.type != END && (curToken.type == MUL || curToken.type == DIV)){
	    if(curToken.type == MUL){
	      eat(MUL);
	      f *= factor();
	    }else if (curToken.type == DIV){
		    eat(DIV);
	      f /= factor();
	    }
	  }
          return f;
	}
	int expr(){
	  int f = term();
	  while (curToken.type != END && (curToken.type == PLUS || curToken.type == MINUS)){
	    if(curToken.type == PLUS){
	      eat(PLUS);
	      f += term();
	    }else if (curToken.type == MINUS){
		    eat(MINUS);
	      f -= term();
	    }
	  }
         return f;	
	}
	int calc(){
	  return expr();
	}
private:
Lexer lexer;
Token curToken;
};

int main(){
  //string str = "7+3"; 
  string str = "7 + 3 * (10 / (12 / (3 + 1) - 1))";
  Lexer l(str);
  Calc cal(l);
  std::cout<<cal.expr()<<std::endl;
}
