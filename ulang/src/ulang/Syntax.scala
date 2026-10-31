package ulang

type Name = String

// source positions, which are not part of the equality and the patterns of nodes
type Pos = scala.util.parsing.input.Position

val nowhere: Pos = scala.util.parsing.input.NoPosition

enum Type:
  case Hole
  case Meta(index: Int)
  case Param(name: Name)
  case Sort(name: Name, args: List[Type])

enum Expr:
  def pos: Pos

  case Lit(value: Any)(val pos: Pos = nowhere)
  case Var(name: Name, typ: Type)(val pos: Pos = nowhere)
  case App(fun: Expr, args: List[Expr])(val pos: Pos = nowhere)
  case Lam(bound: List[Pat.Var], body: Expr)(val pos: Pos = nowhere)
  case Let(bound: List[Pat.Var], args: List[Expr], body: Expr)(val pos: Pos = nowhere)
  case Ite(test: Expr, left: Expr, right: Expr)(val pos: Pos = nowhere)
  case Match(arg: Expr, cases: List[Case[Expr]])(val pos: Pos = nowhere)
  case Comp(result: Expr, bound: List[Pat.Var], lists: List[Expr], test: Expr)(val pos: Pos = nowhere)

enum Pat:
  def pos: Pos

  case Wild()(val pos: Pos = nowhere)
  case Lit(value: Any)(val pos: Pos = nowhere)
  case Var(name: Name, typ: Type)(val pos: Pos = nowhere)
  case App(name: Name, args: List[Pat])(val pos: Pos = nowhere)

case class Case[A](pat: Pat, body: A)

enum Stmt:
  def pos: Pos

  case Skip()(val pos: Pos = nowhere)
  case Assign(vars: List[Expr.Var], args: List[Expr])(val pos: Pos = nowhere)
  case Block(locals: List[Pat.Var], stmts: List[Stmt])(val pos: Pos = nowhere)
  case If(test: Expr, left: Stmt, right: Stmt)(val pos: Pos = nowhere)
  case While(test: Expr, body: Stmt)(val pos: Pos = nowhere)
  case Call(proc: Name, ins: List[Expr], outs: List[Expr.Var], refs: List[Expr.Var])(val pos: Pos = nowhere)
  case Match(arg: Expr, cases: List[Case[Stmt]])(val pos: Pos = nowhere)
  case Assert(test: Expr)(val pos: Pos = nowhere)

case class Constr(name: Name, args: List[Type])

enum Fixity:
  case Prefix,
    Postfix,
    Infix,
    Left,
    Right

enum Cmd:
  def pos: Pos

  case Sort(name: Name, params: List[Name], typ: Type)(val pos: Pos = nowhere)
  case Data(name: Name, params: List[Name], constrs: List[Constr])(val pos: Pos = nowhere)
  case Fun(name: Name, params: List[Name], bound: List[Pat.Var], res: Type, body: Expr)(val pos: Pos = nowhere)
  case Proc(name: Name, params: List[Name], ins: List[Pat.Var], outs: List[Pat.Var], refs: List[Pat.Var], body: Stmt)(
      val pos: Pos = nowhere
  )
  case Import(name: Name)(val pos: Pos = nowhere)
  case Notation(fixity: Fixity, prec: Int, names: List[Name])(val pos: Pos = nowhere)
  case Eval(expr: Expr)(val pos: Pos = nowhere)
  case Run(body: Stmt)(val pos: Pos = nowhere)
  case Test(body: Stmt)(val pos: Pos = nowhere)

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
