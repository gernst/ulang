package ulang

// aliases are sort definitions with their parameters,
// funs and procs are the type schemes of functions with their parameters and the signatures of procedures,
// with parameters, ins, outs, and refs,
// datas are the datatypes with their numbers of parameters, and members their constructors,
// constrs are the constructors with the parameters of their datatype, their argument types, and their datatype
case class Ctx(
    aliases: Map[Name, (List[Name], Type)] = Map(),
    funs: Map[Name, List[(List[Type], Type)]] = Map(),
    procs: Map[Name, (List[Type], List[Type], List[Type], List[Type])] = Map(),
    datas: Map[Name, Int] = Map(),
    members: Map[Name, List[Name]] = Map(),
    constrs: Map[Name, (List[Type], List[Type], Type)] = Map()
)
