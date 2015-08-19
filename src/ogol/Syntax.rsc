module ogol::Syntax

import ParseTree;
import vis::Figure;
import vis::ParseTree;
import vis::Render;

/*

Ogol syntax summary

Program: Command...

Command:
 * Control flow: 
  if Expr Block
  ifelse Expr Block Block
  while Expr Block
  repeat Expr Block
 * Drawing (mind the closing semicolons)
  forward Expr; fd Expr; back Expr; bk Expr; home;
  right Expr; rt Expr; left Expr; lt Expr; 
  pendown; pd; penup; pu;
 * Procedures
  definition: to Name [Var...] Command... end
  call: Name Expr... ;
 
Block: [Command...]
 
Expressions
 * Variables :x, :y, :angle, etc.
 * Number: 1, 2, -3, 0.7, -.1, etc.
 * Boolean: true, false
 * Arithmetic: +, *, /, -
 * Comparison: >, <, >=, <=, =, !=
 * Logical: &&, ||

Reserved keywords
 if, ifelse, while, repeat, forward, back, right, left, pendown, 
 penup, to, true, false, end

Bonus:
 - add literal for colors
 - support setpencolor

*/

start syntax Program = Command*; 


syntax FunDef = "to" FunId id VarId* varIds Command* "end";
syntax FunCall = FunId Expr* ";";

syntax Expr = var: VarId
			| number: Number 
			| boolean: Boolean
			> left (div: Expr "/" Expr 
				| mul: Expr "*" Expr)
			> left (add: Expr "+" Expr
				| min: Expr "-" Expr)
			> gt: Expr "\>" Expr
			| lt: Expr "\<" Expr
			| gteq: Expr "\>=" Expr
			| lteq: Expr "\<=" Expr
			| eq: Expr "=" Expr
			| neq: Expr "!=" Expr
			> left ( and: Expr "&&" Expr 
				| or: Expr "||" Expr);
				
lexical Number = "-"?[0-9]+("."[0-9]+)? | "-"?"."[0-9]+;

syntax Boolean = "true" | "false";

syntax Command = ifStat: 	  "if" Expr Block
				| ifElseStat: "ifelse" Expr Block Block
				| whileStat:  "while" Expr Block
				| repeatStat: "repeat" Expr Block
				
				| "forward" Expr ";" | "fd" Expr ";"
				| "back" Expr ";"    | "bk" Expr ";"
				| "right" Expr ";"   | "rt" Expr ";"
				| "left" Expr ";"    | "lt" Expr ";"
				| "home" ";"
				
				| "penup" ";" | "pu" ";" 
				| "pendown" ";" | "pd" ";"
				| funDef: FunDef
				| funCall: FunCall;
				
syntax Block = "[" Command* "]";
				
keyword Reserved = "if" | "ifelse" | "while" | "repeat" | "forward" | "fd" | "back" | "bk" |
				   "right" | "rt" | "left" | "lt" | "pendown" | "pd" | "penup" | "pu" | 
				   "to" | "true" | "false" | "end" | "home";

lexical VarId = ":" ([a-zA-Z][a-zA-Z0-9]*) \ Reserved !>> [a-zA-Z0-9];
  
lexical FunId =     ([a-zA-Z][a-zA-Z0-9]*) \ Reserved !>> [a-zA-Z0-9];
  

layout Standard 
  = WhitespaceOrComment* !>> [\ \t\n\r] !>> "--";
  
lexical WhitespaceOrComment 
  = whitespace: Whitespace
  | comment: Comment
  ; 
  
lexical Whitespace
  = [\ \t\n\r]
  ;

lexical Comment
  = @category="Comment" "--" ![\n\r]* [\r][\n]
  ;  
  
  
bool testParse(str txt){
 try  return /amb(_) !:= parse(#Expr, txt);
 catch: return false;
}

bool testParseLoc(loc location){
 try  return /amb(_) !:= parse(#start[Program], location);
 catch: return false;
}


public test bool n1() = testParse("1");
public test bool n2() = testParse("1234567");
public test bool n3() = testParse("-1234567");
public test bool n4() = testParse("-.1234567");
public test bool n5() = testParse("-123534.1234567");
public test bool nf6() = !testParse("-123534.");
public test bool nf7() = !testParse("-");
public test bool nf8() = !testParse("-.");

public test bool v1() = testParse(":v");
public test bool v2() = testParse(":test");
public test bool v3() = testParse(":variable");

public test bool tv1() = testParse(":v+:test");
public test bool tv1() = testParse(":v-:test");

public test bool t0() = testParse("1+1");
public test bool t1() = testParse("1+2+3");
public test bool t1() = testParse("1+2-3");
public test bool t2() = testParse("1+2*3/2");

public test bool t3() = testParse("1\<1");
public test bool t4() = testParse("1\>2");
public test bool t5() = testParse("1\<=2");
public test bool t6() = testParse("1\>=2");
public test bool t7() = testParse("1=2");
public test bool t8() = testParse("1!=2");
// public test bool tf9() = !testParse("1\>=2\>2");

public test bool t9() = testParse("1+1*2\<1*2+3");

public test bool l1() = testParse("false&&true");
public test bool l2() = testParse("false&&true||true");
public test bool l3() = testParse("false||true");
public test bool l4() = testParse("false||1\<2 && 1=1 && false");

public test bool testDashed() = testParseLoc(|project://Ogol/input/dashed.ogol|);
public test bool testOctagon() = testParseLoc(|project://Ogol/input/octagon.ogol|);
//public test bool testPumpkin() = testParseLoc(|project://Ogol/input/pumpkin.ogol|);
public test bool testTest() = testParseLoc(|project://Ogol/input/test.ogol|);
public test bool testTrees() = testParseLoc(|project://Ogol/input/trees.ogol|);

public void render(){
	render(visParsetree(parse(#Expr, "1+1*2\<1*2+3")));
}

public void render(str txt){
	render(visParsetree(parse(#Expr, txt)));
}

public void render(loc location){
	render(visParsetree(parse(#start[Program], location)));
}

public Program parseP(loc location){
	return parse(#start[Program], location);
}


