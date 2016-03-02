package ulang.source

import ulang.syntax._

case class Module(name: String, decls: List[Decl]) {
  def thy = Thy(name, Nil, ???, ???, ???)
}