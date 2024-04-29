package pure

import pure.{Name, Var, *}

case class Procedure(name: Name, params: List[Var], ret: Var)
