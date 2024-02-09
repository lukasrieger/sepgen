package util

package alpha:

    trait Term[E, V <: E]:
        this: E =>

        def free: Set[V]

        def rename(re: Map[V, V]): E

        def subst(su: Map[V, E]): E

        infix def subst(v: V, e: E): E = subst(Map(v -> e))

    trait X[E, V <: E] extends Term[E, V]:
        this: V =>

        def fresh(index: Int): V

        def free: Set[V] = Set(this)

        def rename(re: Map[V, V]): V = re.getOrElse(this, this)

        def subst(su: Map[V, E]): E = su.getOrElse(this, this)


trait Alpha[E <: alpha.Term[E, V], V <: E with alpha.X[E, V]]:
    context =>

    type Term = alpha.Term[E, V]
    type X = alpha.X[E, V]

    trait BindT[A]:
        def bound: Set[V]

        def rename(a: Map[V, V], re: Map[V, V]): A

        def subst(a: Map[V, V], su: Map[V, E]): A

        def avoid(xs: Set[V]): Map[V, V] =
            context.fresh(bound & xs)

        def refresh: A =
            val alpha = avoid(bound)
            rename(alpha, alpha)

        def rename(re: Map[V, V]): A =
            val xs = context.free(re)
            val alpha = avoid(xs)
            rename(alpha, re -- bound ++ alpha)

        def subst(su: Map[V, E]): A =
            val xs = context.free(su)
            val alpha = avoid(xs)
            subst(alpha, su -- bound ++ alpha)

    class Xs(xs: List[V]):
        def rename(re: Map[V, V]): List[V] = xs map (_ rename re)


    class Terms(es: List[E]):
        def free: Set[V] = Set(es flatMap (_.free): _*)

        def rename(re: Map[V, V]): List[E] = es map (_ rename re)

        def subst(su: Map[V, E]): List[E] = es map (_ subst su)

    private var _index: Int = 0

    private def nextIndex: Int =
        _index += 1
        _index

    def id(xs: Iterable[V]): Map[V, V] =
        val ys = xs map (x => (x, x))
        ys.toMap

    def fresh(x: V): V = x fresh nextIndex

    def fresh(xs: Iterable[V]): Map[V, V] =
        val ys = xs map (x => (x, x fresh nextIndex))
        ys.toMap

    def free(es: Iterable[E]): Set[V] =
        val ys = es flatMap (_.free)
        ys.toSet

    def free(xs: Map[V, E]): Set[V] =
        val ys = xs.values flatMap (_.free)
        ys.toSet

    def subst[B <: E](xs: (V, B)*): Map[V, B] =
        xs.toMap

    def subst[B <: E](xs: Iterable[(V, B)]): Map[V, B] =
        xs.toMap

    def subst[B <: E](xs: Iterable[V], ys: Iterable[B]): Map[V, B] =
        require(xs.size == ys.size, "length mismatch")
        val zs = xs zip ys
        zs.toMap

    def compose(inner: Map[V, E], outer: Map[V, E]): Map[V, E] =
        val updated = inner map { case (x, e) =>
            (x, e subst outer)
        }
        updated ++ outer
