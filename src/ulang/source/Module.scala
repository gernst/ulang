package ulang.source

import ulang.syntax.Thy

case class Module(name: String, decls: List[Decl]) {
  def thy: Thy = ???
}