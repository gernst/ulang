package ulang

type Name = String

enum Type:
  case Hole
  case Meta(index: Int)
  case Param(name: Name)
  case Sort(name: Name, args: List[Type])

enum Expr:
  case Lit(value: Any)
  case Var(name: Name, typ: Type)
  case App(fun: Expr, args: List[Expr])
  case Lam(bound: List[Pat.Var], body: Expr)
  case Let(bound: List[Pat.Var], args: List[Expr], body: Expr)
  case Ite(test: Expr, left: Expr, right: Expr)
  case Match(arg: Expr, cases: List[Case[Expr]])

enum Pat:
  case Wild
  case Lit(value: Any)
  case Var(name: Name, typ: Type)
  case App(name: Name, args: List[Pat])

case class Case[A](pat: Pat, body: A)

enum Stmt:
  case Skip
  case Assign(vars: List[Expr.Var], args: List[Expr])
  case Block(locals: List[Pat.Var], stmts: List[Stmt])
  case If(test: Expr, left: Stmt, right: Stmt)
  case While(test: Expr, body: Stmt)
  case Call(proc: Name, ins: List[Expr], outs: List[Expr.Var], refs: List[Expr.Var])
  case Match(arg: Expr, cases: List[Case[Stmt]])

case class Constr(name: Name, args: List[Type])

enum Cmd:
  case Sort(name: Name, params: List[Name], typ: Type)
  case Data(name: Name, params: List[Name], constrs: List[Constr])
  case Fun(name: Name, bound: List[Pat.Var], res: Type, body: Expr)
  case Proc(name: Name, ins: List[Pat.Var], outs: List[Pat.Var], refs: List[Pat.Var], body: Stmt)
  case Eval(expr: Expr)
  case Test(lhs: Expr, rhs: Expr)

case class Program(cmds: List[Cmd])

extension (xs: List[Expr.Var])
  @scala.annotation.targetName("Vars.names")
  def names: List[Name] =
    xs.map(_.name)
  @scala.annotation.targetName("Vars.types")
  def types: List[Type] =
    xs.map(_.typ)

extension (xs: List[Pat.Var])
  @scala.annotation.targetName("Pats.names")
  def names: List[Name] =
    xs.map(_.name)
  @scala.annotation.targetName("Pats.types")
  def types: List[Type] =
    xs.map(_.typ)
