package in.tamchow.LinearAlgebra

import scala.language.implicitConversions

object LinearAlgebra {

  object Enhancements {

    implicit class ScalarMatrixInterOp[T](scalar: T)(implicit evidence: Numeric[T]) {
      def product[B: Numeric](matrix: Matrix[B]): Matrix[Double] = matrix product scalar

      def *[B: Numeric](matrix: Matrix[B]): Matrix[Double] = product(matrix)

      def toMatrix: Matrix[T] = Matrix(IndexedSeq(IndexedSeq(scalar)))
    }

    def doSafe[A, B](action: => Either[Throwable, A])(onSuccess: A => B): B =
      action match {
        case Left(exception) => throw exception
        case Right(data) => onSuccess(data)
      }

    implicit def getSafe[A](action: => Either[Throwable, A]): A =
      doSafe(action)(identity _)
  }

  import Enhancements._

  case class Matrix[T: Numeric[T]](private val data: IndexedSeq[IndexedSeq[T]])(implicit numeric: Numeric[T]) {

    private lazy val notSquareUndefinedOperationMessage = s"Matrix of $rows rows & $columns columns is not square, %s cannot be defined"

    import numeric._

    lazy val dimensions: (Int, Int) = (data.length, data.headOption.getOrElse(IndexedSeq()).length)
    lazy val (rows, columns) = dimensions

    override lazy val toString: String =
      data.map(_.mkString("[", ", ", "]"))
        .mkString(s"${getClass.getSimpleName}($rows, $columns) = [", ", ", "]")

    lazy val toPrintableString: String =
      data.map(_.mkString("[", ", ", "]"))
        .mkString("", "\n", "\n")

    private lazy val isValidMatrix: Boolean =
      rowVectors.tail
        .map(row => row.length)
        .forall(rowLength => rowLength == columns)

    def apply(i: Int, j: Int): T = get(i, j)

    /**
      * Accesses an element at (i, j)
      *
      * @param i row index, 1 - based
      * @param j column index, 1 - based
      * @return the element @ (i, j)
      */
    def get(i: Int, j: Int): Either[IndexOutOfBoundsException, T] = checkBoundsException(data(i - 1)(j - 1))(i, j)

    /**
      * Updates an element at (i, j)
      *
      * @param i     row index, 1 - based
      * @param j     column index, 1 - based
      * @param value the value to update
      * @return A matrix with the value @ (i, j) updated to `value`
      */
    def updated(i: Int, j: Int)(value: T): Either[IndexOutOfBoundsException, Matrix[T]] =
      checkBoundsException(Matrix(data.updated(i, data(i - 1).updated(j - 1, value))))(i, j)

    def set(i: Int, j: Int)(value: T): Matrix[T] = getSafe(updated(i, j)(value))

    lazy val isSquare: Boolean = rows == columns

    def sum(matrix: Matrix[T]): Either[UnsupportedOperationException, Matrix[T]] =
      if (matrix.rows == rows || matrix.columns == columns)
        Right(Matrix(
                      for ((ourRow, theirRow) <- data.zip(matrix.data)) yield {
                        for ((ourElement, theirElement) <- ourRow.zip(theirRow)) yield {
                          theirElement + ourElement
                        }
                      }))
      else
        Left(new UnsupportedOperationException(
                                                s"Unequal matrix dimensions, cannot add (${matrix.rows} to $rows rows, ${matrix.columns} to $columns columns"))

    def +(matrix: Matrix[T]): Matrix[T] = sum(matrix)

    def unary_- = Matrix(data.map(row => row.map(element => -element)))

    def difference(matrix: Matrix[T]): Either[UnsupportedOperationException, Matrix[T]] = sum(-matrix)

    def -(matrix: Matrix[T]): Matrix[T] = difference(matrix)

    /**
      * Product of a matrix with a scalar. Overloaded with matrix product for convenience
      *
      * @param scalar   The scalar to multiply this matrix by
      * @param evidence Implicit parameter containing type which allows numeric operations on `scalar`
      * @tparam B The type of `scalar`
      * @return the product of this matrix with the scalar `scalar`
      * @see [[Matrix.*[B](scalar:B)*]]
      */
    def product[B](scalar: B)(implicit evidence: Numeric[B]): Matrix[Double] = {
      val scalarAsDouble = scalar.toString.toDouble
      Matrix(for (row <- rowVectors) yield {
        row.map(_.toDouble * scalarAsDouble)
      })
    }

    /**
      * Operator alias for [[Matrix.product[B](scalar:B)*]]
      *
      * @see [[Matrix.product[B](scalar:B)*]]
      */
    def *[B](scalar: B)(implicit evidence: Numeric[B]): Matrix[Double] = product(scalar)

    def product(matrix: Matrix[T]): Either[IllegalArgumentException, Matrix[T]] =
      if (this.columns != matrix.rows)
        Left(new IllegalArgumentException(
                                           s"Product is not defined for $rows rows of left matrix not equal to $columns columns of right matrix"))
      else
        Right(Matrix(IndexedSeq.tabulate(rows)(row =>
                                                 IndexedSeq.tabulate(matrix.columns)(column =>
                                                                                       Seq.tabulate(columns)(subIndex =>
                                                                                                               data(row)(subIndex) * matrix.data(subIndex)(column)).sum))))

    def *(matrix: Matrix[T]): Matrix[T] = product(matrix)

    lazy val transpose: Matrix[T] =
      Matrix(IndexedSeq.tabulate(columns) { row =>
        IndexedSeq.tabulate(rows) {
                                    column => data(column)(row)
                                  }
                                          })

    def coFactor(i: Int, j: Int): Either[IndexOutOfBoundsException, Matrix[T]] =
      checkBoundsException(Matrix(
                                   for ((row, rowIndex) <- data.zipWithIndex if rowIndex != (i - 1)) yield {
                                     (for ((element, columnIndex) <- row.zipWithIndex if columnIndex != (j - 1)) yield element).toIndexedSeq
                                   }
                                 ))(i, j)

    lazy val determinant: Either[UnsupportedOperationException, T] =
      errorIfNotSquare(
                        getSafe(order) match {
                          case 1 => this (1, 1)
                          case 2 => this (1, 1) * this (2, 2) - this (1, 2) * this (2, 1)
                          case _ => expandAlongTopRow.sum
                        },
                        "determinant")

    private def errorIfNotSquare[A](action: => A, operation: String): Either[UnsupportedOperationException, A] =
      if (isSquare) Right(action) else Left(new UnsupportedOperationException(notSquareUndefinedOperationMessage.format(operation)))

    lazy val expandAlongTopRow: IndexedSeq[T] = IndexedSeq.tabulate(order) {
                                                                             // Craziness mixing "one" and "1" is to avoid even more "implicit" confusion
                                                                             column => this (1, column + 1) * (if (column % 2 != 0) -one else one) * coFactor(1, column + 1).determinant
                                                                           }

    lazy val matrixOfMinors: Matrix[T] =
      Matrix(IndexedSeq.tabulate(rows) {
                                         row =>
                                           IndexedSeq.tabulate(columns) {
                                                                          column => coFactor(row + 1, column + 1).determinant // Won't throw any exceptions! Promise!
                                                                        }
                                       })

    lazy val inverse: Either[UnsupportedOperationException, Matrix[Double]] =
      errorIfNotSquare(matrixOfMinors.transpose * (one.toDouble / getSafe(determinant).toDouble), "inverse")

    lazy val order: Either[UnsupportedOperationException, Int] =
      if (isSquare)
        Right(rows)
      else
        Left(new UnsupportedOperationException(notSquareUndefinedOperationMessage.format("order")))

    lazy val (rowVectors, columnVectors) = (data, transpose.data)

    private def checkBoundsException[A](stuff: => A)(i: Int, j: Int): Either[IndexOutOfBoundsException, A] =
      raiseExceptionOnBoundsViolation(i, j) match {
        case Some(boundsException) => Left(boundsException)
        case None => Right(stuff)
      }

    private def raiseExceptionOnBoundsViolation(i: Int, j: Int): Option[IndexOutOfBoundsException] =
      if (i <= 0 || j <= 0 || i > rows || j > columns)
        Some(new IndexOutOfBoundsException(
                                            s"($i, $j) are invalid 1-based indices for a matrix of $rows rows and $columns columns"))
      else None
  }

  object Matrix {

    def nullMatrix(rows: Int, columns: Int): Matrix[Double] = fill(rows, columns)(0.0)

    def apply(data: String): Either[IllegalArgumentException, Matrix[Double]] =
      if (data.startsWith("[[[") || data.startsWith("]]]"))
        Left(new IllegalArgumentException("Tensors of order > 2 not supported"))
      else if (data.count(_ == '[') != data.count(_ == ']'))
             Left(new IllegalArgumentException("Unbalanced square brackets"))
      else
        Right(Matrix(
                      (if (data.startsWith("[[") && data.endsWith("]]"))
                         data.substring(1, data.length - 1) // trim matrix-delimiting square brackets
                       else data).split("\\],?\\s+\\[") map {
                        // split into rows
                        row =>
                          row.filter(character => !(character == '[' || character == ']')) // remove stray delimiters
                            .split(",?\\s+") // split into elements
                            .map(_.toDouble).toIndexedSeq
                      }))

    def createMatrix[T](data: IndexedSeq[IndexedSeq[T]])(implicit numeric: Numeric[T]): Either[IllegalArgumentException, Matrix[T]] = {
      val matrix = new Matrix(data)
      if (!matrix.isValidMatrix) Left(new IllegalArgumentException("Matrix may not have jagged rows"))
      else Right(matrix)
    }

    def fromString(data: String): Matrix[Double] = getSafe(Matrix(data))

    def identityMatrix(order: Int): Matrix[Double] =
      tabulate(order, order)((row, column) => if (row == column) 1.0 else 0.0)

    def tabulate[T: Numeric](numRows: Int, numColumns: Int)(elementFunction: (Int, Int) => T): Matrix[T] =
      Matrix(IndexedSeq.tabulate(numRows, numColumns)(elementFunction))

    def fill[T: Numeric](numRows: Int, numColumns: Int)(element: => T): Matrix[T] =
      Matrix(IndexedSeq.fill(numRows, numColumns)(element))
  }

  case class Vector(private val components: IndexedSeq[Double]) {
    private val data: Matrix[Double] = Matrix(IndexedSeq(components))
    lazy val dimensions: Int = components.length

    override lazy val toString: String = s"$getClass(${components.mkString(", ")}"

    def add(other: Vector): Vector = Vector((data + other.data).rowVectors(0))

    def +(other: Vector): Vector = add(other)

    def subtract(other: Vector): Vector = Vector((data + other.data).rowVectors(0))

    def -(other: Vector): Vector = subtract(other)

    def crossProduct(other: Vector): Vector = Vector(Matrix(IndexedSeq(IndexedSeq.fill(dimensions)(1.0), components, other.components)).expandAlongTopRow)

    def dotProduct(other: Vector): Double =
      components.zipAll(other.components, 0.0, 0.0)
        .map { case (x1, x2) => x1 * x2 }
        .sum

    def apply(index: Int): Double = components(index)
  }

  object Vector {
    def nullVector(numComponents: Int): Vector =
      fill(numComponents)(0.0)

    def unitVector(numComponents: Int, direction: Int): Vector =
      Vector(IndexedSeq.tabulate(numComponents)(index => if (index == direction) 1.0 else 0.0))

    def tabulate(numComponents: Int)(elementFunction: Int => Double): Vector =
      Vector(IndexedSeq.tabulate(numComponents)(elementFunction))

    def fill(numComponents: Int)(element: => Double): Vector =
      Vector(IndexedSeq.fill(numComponents)(element))

    def apply(components: Double*): Vector =
      Vector(components.toIndexedSeq)
  }

}