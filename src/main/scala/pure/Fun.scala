package pure




def head(of: Expr): Expr = App(fun = Name("head"), args = List(of))
def tail(of: Expr): Expr = App(fun = Name("tail"), args = List(of))
