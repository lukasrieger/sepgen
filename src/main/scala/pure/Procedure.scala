package pure


case class ProcSignature(name: Name, params: List[Var])

case class Procedure(signature: ProcSignature, body: Program)
