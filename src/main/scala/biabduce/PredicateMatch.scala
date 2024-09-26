package biabduce

import biabduce.Spatial.*

/*
  If this returns None -> p1 entails p2 fully
  If this returns Some -> p1 misses some entailments for p2, the generated predicate
  is the missing part
 */
def predMatch(p1: Pred, p2: Pred)
             (using specTable: SpecTable, procTable: ProcTable): Boolean =

  val (p1Base, p1Ind) = p1.splitCasesPost
  val (p2Base, p2Ind) = p2.splitCasesPre

  println("Base cases to prove:")
  println(s"POST: $p1Base")
  println(s"Pre next: $p2Base")

  println("INDUCTIVE CASES TO PROVE ")
  println(s"POST $p1Ind")
  println(s"PRE $p2Ind")
  // First check if p1Base |- p2Base

  val baseResult = p1Base |- p2Base match
    case Some((substL, substR)) if substL.nonEmpty && substR.nonEmpty =>
      sys.error(s"Cant handle this case yet!: ${(substL, substR)}")
    case Some(_) => true
    case None => false

  // For the rest: Forall p in p1Base there exists some p in p2Base that is entailed.

  val inductiveResult = p1Ind.forall: p1 =>
    p2Ind.exists: p2 =>
      p1 |- p2 match
        case Some((substL, substR)) if substL.nonEmpty && substR.nonEmpty =>
          println("Entailment hold!")
          // TODO: What to do with instantiations here?
          true
        case Some(_) => true
        case None =>
          // Try another recursive unfold or give up
          val tt = for
            indCase <- p1Ind
            p1_ = p1.addPiSigma(indCase.pi, indCase.sigma)
          yield p1_

          println("Once unfolded failed, check to see if double unfolding helps:")
          println("Working with :: ")
          println(tt)
          println("To try and prove entailment of ::")
          println(p2)

          tt.exists(p1_ => p1_ |- p2 match
            case Some(value) =>
              // TODO: See above what to do with instantiations
              true
            case None => false
          )



  println(s"DOES BASE OF p1 (${p1.name}) ENTAIL BASE OF p2 (${p2.name})? :- $baseResult")
  println(s"DO INDUCTIVE CASES OF p1 (${p1.name}) ENTAIL BASE OF p2 (${p2.name})? :- $inductiveResult")
  baseResult && inductiveResult


extension (pred: Pred)
  def fn(specs: List[Prop]): (Prop, List[Prop]) =
    val (inductiveCases, base) = specs.partition: p =>
      p.sigma.exists:
        case Pred(name, s) if name == pred.name => true
        case _ => false

    val inductiveCases_ = inductiveCases.map: p =>
      p.copy(
        sigma = p.sigma.filterNot:
          case Pred(name, _) if name == pred.name => true
          case _ => false
      )

    // Ensure there is only on base case
//    assert(base.size == 1)

    // Return base + inductive cases with their recursive call removed.
    (base.head, inductiveCases_)

  def splitCasesPre(using specTable: SpecTable, procTable: ProcTable): (Prop, List[Prop]) =
    val specs = specTable lookupSpec pred.name
    val preSpecs = specs.map(_.pre)

    println(s"GOT PRE CASES OF ${pred.name}")
    println(preSpecs)

    // Return base + inductive cases with their recursive call removed.
    fn(preSpecs)



  def splitCasesPost(using specTable: SpecTable, procTable: ProcTable): (Prop, List[Prop]) =
    val specs = specTable lookupSpec pred.name
    val postSpecs = specs.map(_.post)

    // Return base + inductive cases with their recursive call removed.
    fn(postSpecs)