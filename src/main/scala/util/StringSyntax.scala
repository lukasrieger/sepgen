package util


object StringSyntax:
  val sub = "₀₁₂₃₄₅₆₇₈₉"

  extension (self: String)
    def __(index: Int): String =
      self + (index.toString map (n => sub(n - '0')))

    infix def __(index: Option[Int]): String = index match
      case None => self
      case Some(index) => __(index)

    infix def ~~(index: Option[Int]): String = index match
      case None => self
      case Some(index) => self + "$" + index
  
