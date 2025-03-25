package tasks.adts

/*  Exercise 1: 
 *  Complete the implementation of ComplexADT trait below, so that it passes
 *  the test in ComplexTest.
 */

object Ex1ComplexNumbers:

  trait ComplexADT:
    type Complex

    def complex(re: Double, im: Double): Complex

    extension (complex: Complex)
      def re(): Double
      def im(): Double
      def sum(other: Complex): Complex
      def subtract(other: Complex): Complex
      def asString(): String

  object BasicComplexADT extends ComplexADT:

    // Change assignment below: should probably define a case class and use it?
    case class ComplexAdt(real: Double, imaginary: Double)

    type Complex = ComplexAdt

    def complex(re: Double, im: Double): Complex = ComplexAdt(re, im)

    extension (complex: Complex)
      def re(): Double = complex.real
      def im(): Double = complex.imaginary
      def sum(other: Complex): Complex = ComplexAdt(complex.re() + other.re(), complex.im() + other.im())
      def subtract(other: Complex): Complex = ComplexAdt(complex.re() - other.re(), complex.im() - other.im())
      def asString(): String = complex match
        case a if complex.im() == 0.0 => f"${complex.re()}"
        case b if complex.re() > 0 && complex.im() > 0.0 => f"${complex.re()} + ${complex.im()}i"
        case c if complex.re() == 0.0 => f"${complex.im()}i"
        case _ => f"${complex.re()} - ${complex.im() * -1}i"
