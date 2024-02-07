package util

trait Show[A]:
  def show(value: A): String


extension [A: Show] (value: A)
  def show: String = summon[Show[A]].show(value)