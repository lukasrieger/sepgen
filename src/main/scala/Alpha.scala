
package alpha:

  trait Term[E, V <: E]:
    this: E =>

      def free: Set[V]
      def rename(re: Map[V, V]): E
      def subst(su: Map[V, E]): E

      def subst(v: V, e: E): E = subst(Map(v -> e))


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

  trait Bind[A]:
    def bound: Set[V]
    def rename(a: Map[V, V], re: Map[V, V]): A
    def subst(a: Map[V, V], su: Map[V, E]): A



