package ulang

// aliases are sort definitions with their parameters,
// funs and procs are the type schemes of functions with their parameters and the signatures of procedures,
// with parameters, ins, outs, and refs,
// datas are the datatypes with their numbers of parameters, and members their constructors,
// constrs are the constructors with the parameters of their datatype, their argument types, and their datatype,
// where all names are qualified by their module
case class Ctx(
    aliases: Map[Name, (List[Id], Type)] = Map(),
    funs: Map[Name, List[(List[Type], Type)]] = Map(),
    procs: Map[Name, (List[Type], List[Type], List[Type], List[Type])] = Map(),
    datas: Map[Name, Int] = Map(),
    members: Map[Name, List[Name]] = Map(),
    constrs: Map[Name, (List[Type], List[Type], Type)] = Map()
):
  // the declarations of module m
  def of(m: Id): Ctx =
    def own(n: Name): Boolean =
      n.module.contains(m)
    Ctx(
      aliases.filter((n, _) => own(n)),
      funs.filter((n, _) => own(n)),
      procs.filter((n, _) => own(n)),
      datas.filter((n, _) => own(n)),
      members.filter((n, _) => own(n)),
      constrs.filter((n, _) => own(n))
    )

  // the declarations of both, whose names differ since they are qualified by their modules
  def ++(c: Ctx): Ctx =
    Ctx(aliases ++ c.aliases, funs ++ c.funs, procs ++ c.procs, datas ++ c.datas, members ++ c.members, constrs ++ c.constrs)
