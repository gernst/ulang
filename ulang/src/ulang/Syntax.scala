package ulang

// identifiers that never refer to declarations of modules: type parameters, modules, notations, runs, and tests
type Id = String

// a name with the module that declares it, which is None for locals, builtins, and names before checking
case class Name(module: Option[Id], base: Id):
  override def toString: String =
    base

  // the name with its module, to tell names apart
  def qualified: String =
    module.map(m => s"$m.$base").getOrElse(base)

object Name:
  def apply(base: Id): Name =
    Name(None, base)

// names that builtins and the runtime create, of the modules of the library
object Names:
  val list = Name(Some("list"), "List")
  val nil = Name(Some("list"), "Nil")
  val cons = Name(Some("list"), "Cons")
  val option = Name(Some("option"), "Option")
  val none = Name(Some("option"), "None")
  val some = Name(Some("option"), "Some")
  val osError = Name(Some("os"), "OsError")
  // the exceptions of builtins and accessors, which the prelude declares
  val divisionByZero = Name(Some("prelude"), "DivisionByZero")
  val noIndex = Name(Some("prelude"), "NoIndex")
  val noCharacter = Name(Some("prelude"), "NoCharacter")
  val missingField = Name(Some("prelude"), "MissingField")
  val tuple = Name(",")

// source positions, which are not part of the equality and the patterns of nodes
type Pos = scala.util.parsing.input.Position

val nowhere: Pos = scala.util.parsing.input.NoPosition

enum Type:
  case Hole
  case Meta(index: Int)
  case Param(name: Id)
  case Sort(name: Name, args: List[Type])
  // functions take a fixed number of arguments, since there is no currying
  case Fun(args: List[Type], res: Type)
  // procedures as values, (in A, out B, ref C), whose parameters have modes as in declarations
  case Proc(ins: List[Type], outs: List[Type], refs: List[Type])
  // () is the empty tuple, i.e., the unit type
  case Tuple(items: List[Type])

enum Expr:
  def pos: Pos

  case Lit(value: Any)(val pos: Pos = nowhere)
  case Var(name: Name, typ: Type)(val pos: Pos = nowhere)
  case App(fun: Expr, args: List[Expr])(val pos: Pos = nowhere)
  case Lam(bound: List[Pat.Var], body: Expr)(val pos: Pos = nowhere)
  // lambda in x, out y, ref z => body end, which sees the variables around it but assigns only its outs and refs
  case Proc(ins: List[Pat.Var], outs: List[Pat.Var], refs: List[Pat.Var], body: Stmt)(val pos: Pos = nowhere)
  case Let(bound: List[(Pat.Var, Expr)], body: Expr)(val pos: Pos = nowhere)
  case Ite(test: Expr, left: Expr, right: Expr)(val pos: Pos = nowhere)
  case Match(arg: Expr, cases: List[Case[Expr]])(val pos: Pos = nowhere)
  case Comp(result: Expr, bound: List[Pat.Var], lists: List[Expr], test: Expr)(val pos: Pos = nowhere)
  case Tuple(items: List[Expr])(val pos: Pos = nowhere)
  // an exception of type typ, as an expression of any type
  case Raise(arg: Expr, typ: Type)(val pos: Pos = nowhere)

enum Pat:
  def pos: Pos

  case Wild()(val pos: Pos = nowhere)
  case Lit(value: Any)(val pos: Pos = nowhere)
  case Var(name: Name, typ: Type)(val pos: Pos = nowhere)
  case App(name: Name, args: List[Pat])(val pos: Pos = nowhere)
  case Tuple(items: List[Pat])(val pos: Pos = nowhere)

// a case applies if its pattern matches and its guard holds
case class Case[A](pat: Pat, guard: Option[Expr], body: A)

enum Stmt:
  def pos: Pos

  case Skip()(val pos: Pos = nowhere)
  case Assign(pairs: List[(Expr.Var, Expr)])(val pos: Pos = nowhere)
  case Block(locals: List[Pat.Var], stmts: List[Stmt])(val pos: Pos = nowhere)
  case If(test: Expr, left: Stmt, right: Stmt)(val pos: Pos = nowhere)
  case While(test: Expr, body: Stmt)(val pos: Pos = nowhere)
  case Call(proc: Name, ins: List[Expr], outs: List[Expr.Var], refs: List[Expr.Var])(val pos: Pos = nowhere)
  case Match(arg: Expr, cases: List[Case[Stmt]])(val pos: Pos = nowhere)
  case Assert(test: Expr)(val pos: Pos = nowhere)
  case Raise(arg: Expr, typ: Type)(val pos: Pos = nowhere)
  case Try(body: Stmt, handlers: List[Handler])(val pos: Pos = nowhere)

// handles exceptions of type typ that match pat, or of any type if pat is _
case class Handler(typ: Type, pat: Pat, guard: Option[Expr], body: Stmt)

case class Constr(name: Name, args: List[Type])

enum Fixity:
  case Prefix,
    Postfix,
    Infix,
    Left,
    Right

enum Cmd:
  def pos: Pos

  case Sort(name: Name, params: List[Id], typ: Type)(val pos: Pos = nowhere)
  case Data(name: Name, params: List[Id], constrs: List[Constr])(val pos: Pos = nowhere)
  // functions have at least one list of parameters, and further ones curry them
  case Fun(name: Name, params: List[Id], bound: List[List[Pat.Var]], res: Type, body: Expr)(val pos: Pos = nowhere)
  case Proc(name: Name, params: List[Id], ins: List[Pat.Var], outs: List[Pat.Var], refs: List[Pat.Var], body: Stmt)(
      val pos: Pos = nowhere
  )
  // the first import of a module comes with the module itself, whose commands are those it declares
  case Module(name: Id, cmds: List[Cmd])(val pos: Pos = nowhere)
  case Import(name: Id)(val pos: Pos = nowhere)
  case Notation(fixity: Fixity, prec: Int, names: List[Id])(val pos: Pos = nowhere)
  case Eval(expr: Expr)(val pos: Pos = nowhere)
  case Run(name: Id, body: Stmt)(val pos: Pos = nowhere)
  case Test(name: Id, body: Stmt)(val pos: Pos = nowhere)

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
