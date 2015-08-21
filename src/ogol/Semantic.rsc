module ogol::Semantic

import ogol::Syntax;
import ogol::Desugar;
import ParseTree;

import Set;
import Relation;
import Map;

alias Fun = str;

alias Funs = set[Fun];
alias Calls = rel[Fun, Fun];
                       
// Call graph -> <functie a, functie b>


// Call graph extractor

Funs getFunctions(Program p)
 = domain((unparse(f.id): f | /FunDef f := p));
 
test bool testGetFunctions()
	= getFunctions(desugar(parse(#start[Program], |project://Ogol/input/octagon.ogol|).top))
	== {"dash", "octagon", "squareDash", "squareDashTwirl"};

Funs getUnusedFunctions(Program p)
	= getFunctions(p) - range(getCallGraph(p));

test bool testGetUnusedFunctions()
	= getUnusedFunctions(desugar(parse(#start[Program], |project://Ogol/input/octagon.ogol|).top))
	== {"octagon"};
	
Funs getReachableFunctions(Fun fromFun, Program p)
	= (getCallGraph(p)+)[fromFun];

test bool testGetReachableFunctions()
	= getReachableFunctions("global", desugar(parse(#start[Program], |project://Ogol/input/octagon.ogol|).top))
	== {"squareDash","squareDashTwirl","dash"};
	
// Command
Calls getCallGraph(Program p){
	calls = {};
	
	for(cmd <- p.cmds){
		calls = calls + getCallsFromCommand(cmd, "global");
	}
	
	for(/FunDef f := p){
		calls = calls + getCallsFromFunction(f);
	}
	
	return calls;
}

Calls getCallsFromFunction(FunDef f){
	calls = {};
	
	fromFun = "<f.id>";
	
	for(cmd <- f.cmds){
		calls = calls + getCallsFromCommand(cmd, fromFun);
	}
	
	return calls;
}

// ifelse
Calls getCallsFromCommand((Command)`ifelse <Expr _> <Block b1> <Block b2>`, Fun fromFun){
	return getCallsFromBlock(b1, fromFun) + getCallsFromBlock(b2, fromFun);
}

// repeat
Calls getCallsFromCommand((Command)`repeat <Expr _> <Block b1>`, Fun fromFun){
	return getCallsFromBlock(b1, fromFun);
}

Calls getCallsFromBlock((Block)`[<Command* cmds>]`, Fun fromFun){
	calls = {};
		
	for(cmd <- cmds){
		calls = calls + getCallsFromCommand(cmd, fromFun);
	}
	
	return calls;
}

// FunCall
Calls getCallsFromCommand((Command)`<FunId toId> <Expr* _>;`, Fun fromFun){
	return {<fromFun,"<toId>">}; 
}


default Calls getCallsFromCommand(Command c, Fun fromFun){
	return {};
}


