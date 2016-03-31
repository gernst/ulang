package ulang.source

case class Module(name: String, decls: List[Decl]) {
  override def toString = decls.mkString("\n")
}