package pure

enum Quantifier(val name: String):
  case ForAll extends Quantifier("forall")
  case Exists extends Quantifier("exists")
    