package ulang.transform

object Check {
  import ulang.source
  import ulang.syntax
  
  def check(mod: source.Module): syntax.Thy = {
    syntax.Thy(mod.name, mod.decls map check)
  }
  
  def check(decl: source.Decl): syntax.Decl = {
    ???
  }
}