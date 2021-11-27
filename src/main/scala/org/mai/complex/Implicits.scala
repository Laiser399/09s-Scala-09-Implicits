package org.mai.complex

import scala.language.implicitConversions

case class Complex[A: Arithmetical](re: A, im: A) {
  override def toString: String = s"$re+${im}i"

  val arithmetical: Arithmetical[A] = implicitly[Arithmetical[A]]

  def +(that: Complex[A]): Complex[A] = Complex(
    arithmetical.add(re, that.re),
    arithmetical.add(im, that.im)
  )

  def -(that: Complex[A]): Complex[A] = Complex(
    arithmetical.subtract(re, that.re),
    arithmetical.subtract(im, that.im)
  )
}

trait Arithmetical[A] {
  def add(first: A, second: A): A
  def subtract(first: A, second: A): A
  def zero(): A
}

object Implicits {
  implicit object DoubleArithmetical extends Arithmetical[Double] {
    override def add(first: Double, second: Double): Double = first + second

    override def subtract(first: Double, second: Double): Double = first - second

    override def zero(): Double = 0d
  }

  implicit object IntArithmetical extends Arithmetical[Int] {
    override def add(first: Int, second: Int): Int = first + second

    override def subtract(first: Int, second: Int): Int = first - second

    override def zero(): Int = 0
  }

  implicit def tuple2ToComplex[A: Arithmetical](arg: (A, A)): Complex[A] =
    Complex(arg._1, arg._2)

  implicit def arithmeticalToComplex[A: Arithmetical](arg: A): Complex[A] = {
    val arithmetical = implicitly[Arithmetical[A]]
    Complex(arg, arithmetical.zero())
  }

  implicit class ComplexExtensions[A: Arithmetical](arg: A) {
    private val arithmetical = implicitly[Arithmetical[A]]

    def real(): Complex[A] = {
      Complex(arg, arithmetical.zero())
    }

    def imaginary(): Complex[A] = {
      Complex(arithmetical.zero(), arg)
    }
  }
}
