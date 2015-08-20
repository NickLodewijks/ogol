module ogol::Desugar

import ogol::Syntax;

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