import cats.Foldable
import monocle.Monocle.universe
import pure.Program.Call
import pure.{Assert, Name, Predicate, Procedure}

import scala.annotation.tailrec

case class ProcAssert(pre: Predicate, post: Predicate)

case class InferenceContext(context: Map[Name, ProcAssert])

def analyze(context: Procedure*)(proc: Procedure): (Predicate, InferenceContext) =
  val ordered = toDependencyGraph((context :+ proc).toSet)
  ???


private def toDependencyGraph(procedures: Set[Procedure]): List[Procedure] =
  @tailrec
  def topologicalSort(deps: Map[Procedure, Set[Procedure]], done: List[Procedure]): List[Procedure] =
    val (noPredecessors, hasPredecessors) = deps.partition(_._2.isEmpty)
    if (noPredecessors.isEmpty)
      if (hasPredecessors.isEmpty) done else sys.error(hasPredecessors.toString)
    else
      val found = noPredecessors.keys
      topologicalSort(hasPredecessors.view.mapValues { _ -- found }.toMap, done ++ found)

  topologicalSort(
    deps = procedures.map(collectDependencies(_, procedures)).toMap,
    done = List()
  )


private def collectDependencies(procedure: Procedure, context: Set[Procedure]): (Procedure, Set[Procedure]) =
  procedure -> Foldable[LazyList].foldMap(universe(procedure.body)):
    case Call(name, _, _) if name != procedure.signature.name => Set(context.find(_.signature.name == name).get)
    case _ => Set.empty

