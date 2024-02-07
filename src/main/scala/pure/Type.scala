package pure

import util.Alpha

sealed trait Type extends Type.Term

case class Param(name: Name) extends Type with Type.X:
  def fresh(index: Int): Param = Param(name withIndex index)

  def in(typ: Type): Boolean = ???
  
  def in(types: List[Type]): Boolean = types exists (this in _)

object Type extends Alpha[Type, Param] {
  def prune(su: Map[Param, Type]): Map[Param, Type] = ???

  def prune(typ: Type, su: Map[Param, Type]): Type = ???

  def unify(typ1: Type, typ2: Type, su: Map[Param, Type]): Map[Param, Type] = ???

  def unify(
             types1: List[Type],
             types2: List[Type],
             su: Map[Param, Type]
           ): Map[Param, Type] = ???

  def bind(
            typ1: Type,
            typ2: Type,
            su: Map[Param, Type] = Map()
          ): Map[Param, Type] = ???

  def unify(
             types1: List[Type],
             res1: Type,
             types2: List[Type],
             res2: Type,
             su: Map[Param, Type]
           ): Map[Param, Type] = ???

  def binds(
             types1: List[Type],
             res1: Type,
             types2: List[Type],
             res2: Type,
             su: Map[Param, Type]
           ): Map[Param, Type] = ???

  def binds(
             types1: List[Type],
             types2: List[Type],
             su: Map[Param, Type] = Map()
           ): Map[Param, Type] = ???
  
}

class TypeList(types: List[Type]) extends Type.Terms(types)