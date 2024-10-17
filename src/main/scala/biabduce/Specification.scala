package biabduce

import biabduce.Expression.LogicalVar
import biabduce.Spatial.*
import pure.Name


type SymbolicSpec = (Symbolic, Symbolic)

extension (symSpec: SymbolicSpec)
  def show: String =
    s"${symSpec._1} => ${symSpec._2}"

case class Specification(
                        pre: Prop,
                        post: Prop
                        ):

  override def toString: String =
    s"Pre: ${pre}\n Post: ${post}"


  def existQuantify: SymbolicSpec =
    def collectLogVars(prop: Prop): Set[LogicalVar] =
      prop.sigma.foldLeft(Set.empty): (set, sigma) =>
        sigma match
          case PointsTo(ptr: LogicalVar, _, cell: LogicalVar) => set + ptr + cell
          case PointsTo(ptr: LogicalVar, _, _) => set + ptr
          case PointsTo(_, _, cell: LogicalVar) => set + cell
          case _ => set
    
    val freeVarsPre = collectLogVars(pre)
    val freeVarsPost = collectLogVars(post)
    
    Symbolic.Exists(freeVarsPre.toList, pre) -> Symbolic.Exists(freeVarsPost.toList, post)
  
  
  def renameVars: Specification =
    this

type SpecTable = scala.collection.mutable.Map[Name, List[Specification]]

extension (specTable: SpecTable)
  infix def lookupSpec(name: Name)(using procTable: ProcTable): List[Specification] =
    given s: SpecTable = specTable
    specTable.getOrElse(name, symExeProc(procTable(name)))
    
  infix def resolvePred(pred: Spatial.Pred): PropSet =
    specTable(pred.name).instantiate(pred.params)
    


extension (specs: List[Specification])
  def toPropSet: PropSet = ???
  
  def instantiate(args: List[Expression]): PropSet =
    specs.map(_.post).toSet

