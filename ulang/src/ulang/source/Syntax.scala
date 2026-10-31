package ulang
package source

// the syntax as written, before operators, sugar, and imports are resolved, named like the core syntax,
// where each node has the position of its first token

type Name = String

type Pos = scala.util.parsing.input.Position

// operands and operators of an expression, type, or pattern, before their precedence is known
enum Item[A]:
  case Arg(a: A)
  case Op(name: Name, pos: Pos)

enum Type:
  case Sort(name: Name, args: List[Type], pos: Pos)
  case Tuple(types: List[Type], pos: Pos)
  case Ops(items: List[Item[Type]], pos: Pos)

enum Expr:
  case Lit(value: Any, pos: Pos)
  case Var(name: Name, pos: Pos)
  case App(fun: Expr, args: List[Expr], pos: Pos)
  case Lam(bound: List[Pat.Var], body: Expr, pos: Pos)
  case Let(bound: List[Pat], args: List[Expr], body: Expr, pos: Pos)
  case Ite(test: Expr, left: Expr, right: Expr, pos: Pos)
  case Match(arg: Expr, cases: List[Case[Expr]], pos: Pos)
  case Comp(result: Expr, bound: List[Pat.Var], lists: List[Expr], test: Option[Expr], pos: Pos)
  case Tuple(exprs: List[Expr], pos: Pos)
  case Elems(exprs: List[Expr], pos: Pos)
  case Ops(items: List[Item[Expr]], pos: Pos)

enum Pat:
  case Wild(pos: Pos)
  case Lit(value: Any, pos: Pos)
  case Var(name: Name, typ: Option[Type], pos: Pos)
  case App(name: Name, args: List[Pat], pos: Pos)
  case Tuple(pats: List[Pat], pos: Pos)
  case Ops(items: List[Item[Pat]], pos: Pos)

case class Case[A](pat: Pat, body: A, pos: Pos)

enum Stmt:
  case Skip(pos: Pos)
  case Assign(vars: List[Expr.Var], args: List[Expr], pos: Pos)
  case Block(locals: List[Pat.Var], stmts: List[Stmt], pos: Pos)
  case If(test: Expr, left: Stmt, right: Stmt, pos: Pos)
  case While(test: Expr, body: Stmt, pos: Pos)
  case Call(proc: Name, ins: List[Expr], outs: List[Expr.Var], refs: List[Expr.Var], pos: Pos)
  case Match(arg: Expr, cases: List[Case[Stmt]], pos: Pos)
  case Assert(test: Expr, pos: Pos)

case class Constr(name: Name, args: List[Type], pos: Pos)

enum Fixity:
  case Prefix, Postfix, Infix, Left, Right

enum Cmd:
  case Import(name: Name, pos: Pos)
  case Notation(fixity: Fixity, prec: Int, names: List[Name], pos: Pos)
  case Sort(name: Name, params: List[Name], typ: Type, pos: Pos)
  case Data(name: Name, params: List[Name], constrs: List[Constr], pos: Pos)
  case Fun(name: Name, params: List[Name], bound: List[Pat.Var], res: Type, body: Expr, pos: Pos)
  case Proc(name: Name, params: List[Name], ins: List[Pat.Var], outs: List[Pat.Var], refs: List[Pat.Var], body: Stmt, pos: Pos)
  case Eval(expr: Expr, pos: Pos)
  case Run(body: Stmt, pos: Pos)
  case Test(body: Stmt, pos: Pos)

case class Program(cmds: List[Cmd])
