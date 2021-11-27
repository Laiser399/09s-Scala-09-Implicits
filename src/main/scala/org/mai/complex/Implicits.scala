package org.mai.complex

case class Complex[A: Arithmetic](re: A, im: A) {
  override def toString: String = s"$re+${im}i"

  def +(that: Complex[A]) = null
  def -(that: Complex[A]) = null
}

trait Arithmetic[A] {

}

object Implicits {

}
