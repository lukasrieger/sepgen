package pure


case class ProcSignature(name: Name, params: List[Var], returnCount: Int)

case class Procedure(signature: ProcSignature, body: Program)
