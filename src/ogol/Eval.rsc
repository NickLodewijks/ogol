module ogol::Eval

import ogol::Syntax;
import ogol::Canvas;
import ogol::Canvas2JS;
import String;
import ParseTree;
import Prelude;
import util::Math;

alias FunEnv = map[FunId id, FunDef def];

alias VarEnv = map[VarId id, Value val];

data Value
  = boolean(bool b)
  | number(real i)
  ;

/*
         +y
         |
         |
         |
-x ------+------- +x
         |
         |
         |
        -y

NB: home = (0, 0)
*/

alias Turtle = tuple[int dir, bool pendown, Point position];

alias State = tuple[Turtle turtle, Canvas canvas];

Canvas evald(Program p){
	return eval(desugar(p));
}

// Top-level eval function
Canvas eval(p:(Program)`<Command* cmds>`){
	funenv = collectFunDefs(p);
	VarEnv varenv = ();
	State state = <<0, false, <0,0>>, []>;
	
	for(c <- cmds){
	//	println(c);
		state = eval(c, funenv, varenv, state);
	}
	
	return state.canvas;
}

FunEnv collectFunDefs(Program p)
 = ( f.id: f | /FunDef f := p );

// Block
State eval((Block)`[<Command* cmds>]`, 
		FunEnv fenv, VarEnv venv, State state){
		
	for(c <- cmds){
		state = eval(c, fenv, venv, state);
	}
	
	return state;
}

// Command ifElseStat (ifStat is rewritten to ifElseStat)
State eval(c:(Command)`ifelse <Expr c> <Block b1> <Block b2>`, 
		FunEnv fenv, VarEnv venv, State state){
	if(eval(c, venv)){
		return eval(b1, fenv, venv, state);
	} else {
		return eval(b2, fenv, venv, state);
	}
}

// Command while
State eval((Command)`while <Expr c> <Block b>`, 
		FunEnv fenv, VarEnv venv, State state){
		
	while(eval(c, venv)){
		state = eval(b, fenv, venv, state);
	}
	
	return state;
}

// Command repeat
State eval((Command)`repeat <Expr e> <Block b>`, 
		FunEnv fenv, VarEnv venv, State state){
    
	for(i <- [0..eval(e, venv).i]){
		state = eval(b, fenv, venv, state);
	}
	
	return state;
}

// Command forward
State eval((Command)`forward <Expr e>;`, 
		FunEnv fenv, VarEnv venv, State state){
	Point oldPos = state.turtle.position;
	int dir = state.turtle.dir;
		    
	state.turtle.position.x += toInt((cos(dir*(PI()/180)) * eval(e, venv).i));
	state.turtle.position.y += toInt((sin(dir*(PI()/180)) * eval(e, venv).i));
	
	if(state.turtle.pendown){
		state.canvas = state.canvas + line(oldPos, state.turtle.position);
	}
	
	return state;
}

// Command back
State eval((Command)`back <Expr e>;`, 
		FunEnv fenv, VarEnv venv, State state){
	Point oldPos = state.turtle.position;
	int dir = state.turtle.dir;
		    
	state.turtle.position.x -= toInt((cos(dir*(PI()/180)) * eval(e, venv).i));
	state.turtle.position.y -= toInt((sin(dir*(PI()/180)) * eval(e, venv).i));
	
	if(state.turtle.pendown){
		state.canvas = state.canvas + line(oldPos, state.turtle.position);
	}
	
	return state;
}

// Command right
State eval((Command)`right <Expr e>;`, 
		FunEnv fenv, VarEnv venv, State state){
	int dir = state.turtle.dir + (toInt(eval(e, venv).i) % 360);
	
	if(dir < 0){
		dir = 360 - abs(dir);
	}	
		    
	state.turtle.dir = dir;

	return state;
}

// Command left
State eval((Command)`left <Expr e>;`, 
		FunEnv fenv, VarEnv venv, State state){	
	int dir = state.turtle.dir - (toInt(eval(e, venv).i) % 360);
	
	if(dir < 0){
		dir = 360 - abs(dir);
	}	
		    
	state.turtle.dir = dir;

	return state;
}

// Command home
State eval((Command)`home;`,FunEnv fenv, VarEnv venv, State state){
	state.turtle.position = <0,0>;
	
	return state;
}

// Command penup
State eval((Command)`penup;`,FunEnv fenv, VarEnv venv, State state){
	state.turtle.pendown = false;
	return state;
}

// Command pendown
State eval((Command)`pendown;`,FunEnv fenv, VarEnv venv, State state){
	state.turtle.pendown = true;
	return state;
}

// Command funDef
State eval((Command)`to <FunId id> <VarId* varIds> <Command* cmds> end`,
		FunEnv fenv, VarEnv venv, State state){
	return state;
}

// Command funCall
State eval((Command)`<FunId id> <Expr* es>;`, 
		FunEnv fenv, VarEnv venv, State state){
	
	map[VarId, Expr] map1 = (varId : expr | varId <- fenv[id].varIds, expr <- es);
	
	for(varId <- fenv[id].varIds){
		venv[varId] = eval(map1[varId], venv);
	//	println("VarId: <varId>, value: <eval(map1[varId], venv)>");
	}
	
	for(c <- fenv[id].cmds){
		println(c);
		state = eval(c, fenv, venv, state);
	}
	
	
	return state;
}

Program desugar(Program p){
 return visit (p){
 	case (Command)`fd <Expr e>;`
 		=>(Command) `forward <Expr e>;`
 	case (Command)`bk <Expr e>;`
 		=>(Command)`back <Expr e>;`
 	case (Command)`rt <Expr e>;`
 		=>(Command)`right <Expr e>;`
 	case (Command)`lt <Expr e>;`
 		=>(Command)`left <Expr e>;`
 		
 	case (Command)`pd;`
 		=>(Command)`pendown;`
 	case (Command)`pu;`
 		=>(Command)`penup;`
 		
 	case (Expr)`<Expr lhs> \< <Expr rhs>`
 		=>(Expr)`<Expr rhs> \> <Expr lhs>`
 	case (Expr)`<Expr lhs> \<= <Expr rhs>`
 		=>(Expr)`<Expr rhs> \>= <Expr lhs>`
 		
 	case (Command)`if <Expr c> <Block b>`
 		=> (Command)`ifelse <Expr c> <Block b> [ ]`
 }
}

// Expr var
Value eval((Expr)`<VarId x>`, VarEnv env)
    = env[x];
    
test bool testVar()
	= eval((Expr)`:x`, ((VarId)`:x`: number(1.0)))
 	==number(1.0);
 	
// Expr number
Value eval((Expr) `<Number n>`, VarEnv env)
  = number(toReal(unparse(n)));
  
test bool testNumber()
   = eval((Expr) `-1.23`, ())
   == number(-1.23);

// Expr boolean true
Value eval((Expr) `true`, VarEnv env)
 = boolean(true);
 
test bool testTrue() = eval((Expr)`true`, ()) == boolean(true);

// Expr boolean false
Value eval((Expr) `false`, VarEnv env)
 = boolean(false);
 
test bool testFalse() = eval((Expr)`false`, ()) == boolean(false);

// Expr div
Value eval((Expr)`<Expr lhs> / <Expr rhs>`, VarEnv env)
	= number(x / y)
	when
	  number(x) := eval(lhs, env),
	  number(y) := eval(rhs, env);
	  

test bool testDiv()
	= eval((Expr)`:x/2`, ((VarId)`:x`: number(10.0)))
	== number(5.0);


// Expr mul
Value eval((Expr)`<Expr lhs> * <Expr rhs>`, VarEnv env)
	= number(x * y)
	when
	  number(x) := eval(lhs, env),
	  number(y) := eval(rhs, env);
	  
test bool testMul()
	= eval((Expr)`:x*2`, ((VarId)`:x`: number(2.0)))
	== number(4.0);
	
// Expr add
Value eval((Expr)`<Expr lhs> + <Expr rhs>`, VarEnv env)
	= number(x + y)
	when
	 number(x) := eval(lhs, env),
	 number(y) := eval(rhs, env);
	 
test bool testAdd()
	= eval((Expr)`:x+20`, ((VarId)`:x`:number(30.0)))
	== number(50.0);

// Expr min
Value eval((Expr)`<Expr lhs> - <Expr rhs>`, VarEnv env)
	= number(x - y)
	when 
	  number(x) := eval(lhs, env),
	  number(y) := eval(rhs, env);
	  
test bool testMin()
	= eval((Expr)`:x-10`, ((VarId)`:x`:number(20.0)))
	== number(10.0);

// Expr gt (lt is rewritten to gt)
Value eval((Expr)`<Expr lhs> \> <Expr rhs>`, VarEnv env)
	= boolean(x > y)
	when 
     number(x) := eval(lhs, env),
     number(y) := eval(rhs, env);
     
test bool testGt()
	= eval((Expr)`:x\>5`, ((VarId)`:x`:number(10.0)))
	== boolean(true);

// Expr gteq (lteq is rewritten to gteq)
Value eval((Expr)`<Expr lhs> \>= <Expr rhs>`, VarEnv env)
	= boolean(x >= y)
	when
	 number(x) := eval(lhs, env),
	 number(y) := eval(rhs, env);
	 
test bool testGteq()
	= eval((Expr)`:x\>=10`, ((VarId)`:x`:number(10.0)))
	== boolean(true);
test bool testNGteq()
	= eval((Expr)`:x\>=10`, ((VarId)`:x`:number(9.0)))
	== boolean(false);

// Expr eq
Value eval((Expr)`<Expr lhs> = <Expr rhs>`, VarEnv env)
	= boolean(x == y)
	when
	 x := eval(lhs, env),
	 y := eval(rhs, env);
	 
test bool testEq()
	= eval((Expr)`true=:x`, ((VarId)`:x`:boolean(true)))
	== boolean(true);
test bool testNEq()
	= eval((Expr)`true=false`, ())
	== boolean(false);
test bool testEq2()
	= eval((Expr)`1.0=:x`, ((VarId)`:x`:number(1.0)))
	== boolean(true);
test bool testNEq2()
	= eval((Expr)`1.0=2.0`, ())
	== boolean(false);

// Expr neq
Value eval((Expr)`<Expr lhs> != <Expr rhs>`, VarEnv env)
	= boolean(x != y)
	when
	 x := eval(lhs, env),
	 y := eval(rhs, env);
	 
test bool testNeq()
	= eval((Expr)`true!=:x`, ((VarId)`:x`:boolean(false)))
	== boolean(true);
test bool testNeq()
	= eval((Expr)`1.0!=:x`, ((VarId)`:x`:number(2.0)))
	== boolean(true);
test bool testNNeq()
	= eval((Expr)`1.0!=:x`, ((VarId)`:x`:number(1.0)))
	== boolean(false);
	
// Expr and
Value eval((Expr)`<Expr lhs> && <Expr rhs>`, VarEnv env)
	= boolean(x && y)
	when
	 boolean(x) := eval(lhs, env),
	 boolean(y) := eval(rhs, env);
	 
test bool testAnd()
	= eval((Expr)`:x&&true`, ((VarId)`:x`:boolean(false)))
	== boolean(false);
test bool testAnd2()
	= eval((Expr)`:x&&false`, ((VarId)`:x`:boolean(false)))
	== boolean(false);
test bool testAnd3()
	= eval((Expr)`:x&&true`, ((VarId)`:x`:boolean(true)))
	== boolean(true);
test bool testAnd4()
	= eval((Expr)`:x&&false`, ((VarId)`:x`:boolean(true)))
	== boolean(false);

// Expr or
Value eval((Expr)`<Expr lhs> || <Expr rhs>`, VarEnv env)
	= boolean(x || y)
	when
	 boolean(x) := eval(lhs, env),
	 boolean(y) := eval(rhs, env);
	 
test bool testOr()
	= eval((Expr)`:x||true`, ((VarId)`:x`:boolean(false)))
	== boolean(true);
test bool testOr2()
	= eval((Expr)`:x||false`, ((VarId)`:x`:boolean(false)))
	== boolean(false);
test bool testOr3()
	= eval((Expr)`:x||true`, ((VarId)`:x`:boolean(true)))
	== boolean(true);
test bool testOr4()
	= eval((Expr)`:x||false`, ((VarId)`:x`:boolean(true)))
	== boolean(true);
	  
default Value eval(Expr e, VarEnv _){
	throw "Cannot eval: <e>"; 
}

public void octagonJs(){
	compileCanvas(evald(parse(#Program, |project://Ogol/input/octagon.ogol|)), |project://Ogol/input/ogol.js|);
}

public void dashedJs(){
	compileCanvas(evald(parse(#Program, |project://Ogol/input/dashed.ogol|)), |project://Ogol/input/ogol.js|);
}

public void treesJs(){
	compileCanvas(evald(parse(#start[Program], |project://Ogol/input/trees.ogol|).top), |project://Ogol/input/ogol.js|);
}

