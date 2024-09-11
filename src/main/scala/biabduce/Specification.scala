package biabduce

import pure.Name


case class Specification(
                        pre: Prop,
                        post: Prop
                        ):

  /*
  TODO: !!RENAMING!!
   */
  def renameVars: Specification =
    this

type SpecTable = Map[Name, List[Specification]]

extension (specTable: SpecTable)
  infix def lookupSpec(name: Name): List[Specification] =
      specTable(name) map (_.renameVars)