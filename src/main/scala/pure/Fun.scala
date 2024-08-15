package pure




def head(of: Expr): Expr = App(fun = Name("head"), args = List(of))
def tail(of: Expr): Expr = App(fun = Name("tail"), args = List(of))

def head_(of: Var): Var = Var(Name(s"%HEAD$of"))
def tail_(of: Var): Var = Var(Name(s"%TAIL$of"))

