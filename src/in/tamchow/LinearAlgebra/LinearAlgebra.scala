package in.tamchow.LinearAlgebra

object LinearAlgebra {
  implicit def scalarToMatrix[T](scalar: T)(implicit numeric: Numeric[T]): Matrix[T] =
    new Matrix(IndexedSeq(IndexedSeq(scalar)))

  def doSafe[A, B](action: => Either[Throwable, A])(onSuccess: A => B): B = {
    action match {
      case Left(exception) => throw exception
      case Right(data) => onSuccess(data)
    }
  }

  implicit def getSafe[A](action: => Either[Throwable, A]): A = {
    action match {
      case Left(exception) => throw exception
      case Right(data) => data
    }
  }
}

case class Matrix[T](private val data: IndexedSeq[IndexedSeq[T]])(implicit numeric: Numeric[T]) {

  private lazy val notSquareUndefinedOperationMessage = s"Matrix of $rows rows & $columns columns is not square, %s cannot be defined"

  import LinearAlgebra.getSafe
  import numeric._

  lazy val dimension: (Int, Int) = (data.length, data.headOption.getOrElse(IndexedSeq()).length)
  lazy val (rows, columns) = dimension

  override lazy val toString: String = data map {
    row => row mkString("[", ", ", "]")
  } mkString(s"${getClass.getSimpleName}($rows, $columns) = [", ", ", "]")

  lazy val toPrintableString: String = data map {
    row => row mkString("[", ", ", "]")
  } mkString("", "\n", "\n")

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
  def updated(i: Int, j: Int)(value: T): Either[IndexOutOfBoundsException, Matrix[T]] = {
    checkBoundsException(Matrix(data.updated(i, data(i - 1).updated(j - 1, value))))(i, j)
  }

  def set(i: Int, j: Int)(value: T): Matrix[T] = getSafe(updated(i, j)(value))

  lazy val isSquare: Boolean = rows == columns

  def sum(matrix: Matrix[T]): Either[UnsupportedOperationException, Matrix[T]] = {
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
  }

  def +(matrix: Matrix[T]): Matrix[T] = sum(matrix)

  def unary_- = Matrix(data.map(row => row.map(element => -element)))

  def difference(matrix: Matrix[T]): Either[UnsupportedOperationException, Matrix[T]] = sum(-matrix)

  def -(matrix: Matrix[T]): Matrix[T] = difference(matrix)

  def product(matrix: Matrix[T]): Either[IllegalArgumentException, Matrix[T]] = {
    if (this.columns != matrix.rows)
      Left(new IllegalArgumentException(
        s"Product is not defined for $rows rows of left matrix not equal to $columns columns of right matrix"))
    else {
      Right(Matrix(IndexedSeq.tabulate(rows)(row =>
        IndexedSeq.tabulate(matrix.columns)(column =>
          Seq.tabulate(columns)(subIndex =>
            data(row)(subIndex) * matrix.data(subIndex)(column)).sum))))
    }
  }

  def *(matrix: Matrix[T]): Matrix[T] = product(matrix)

  lazy val transpose: Matrix[T] = {
    Matrix(IndexedSeq.tabulate(columns) { row =>
      IndexedSeq.tabulate(rows) {
        column => data(column)(row)
      }
    })
  }

  def coFactor(i: Int, j: Int): Either[IndexOutOfBoundsException, Matrix[T]] = {
    checkBoundsException(Matrix(
      for ((row, rowIndex) <- data.zipWithIndex if rowIndex != (i - 1)) yield {
        (for ((element, columnIndex) <- row.zipWithIndex if columnIndex != (j - 1)) yield element).toIndexedSeq
      }
    ))(i, j)
  }

  lazy val determinant: Either[UnsupportedOperationException, Double] =
    if (isSquare) {
      Right(
        getSafe(order) match {
          case 1 => this (1, 1).toDouble
          case 2 => (this (1, 1) * this (2, 2) - this (1, 2) * this (2, 1)).toDouble
          case x => //expand along top row
            IndexedSeq.tabulate(x) {
              column => (if (column % 2 != 0) -1 else 1) * this (1, column + 1).toDouble * coFactor(1, column + 1).determinant
            }.sum
        }
      )
    }
    else
      Left(new UnsupportedOperationException(notSquareUndefinedOperationMessage.format("determinant")))

  lazy val order: Either[UnsupportedOperationException, Int] =
    if (isSquare)
      Right(rows)
    else
      Left(new UnsupportedOperationException(notSquareUndefinedOperationMessage.format("order")))

  lazy val (rowVectors, columnVectors) = (data, transpose.data)

  private def checkBoundsException[A](stuff: => A)(i: Int, j: Int): Either[IndexOutOfBoundsException, A] = {
    raiseExceptionOnBoundsViolation(i, j) match {
      case Some(boundsException) => Left(boundsException)
      case None => Right(stuff)
    }
  }

  private def raiseExceptionOnBoundsViolation(i: Int, j: Int): Option[IndexOutOfBoundsException] = {
    if (i <= 0 || j <= 0 || i > rows || j > columns)
      Some(new IndexOutOfBoundsException(
        s"($i, $j) are invalid 1-based indices for a matrix of $rows rows and $columns columns"))
    else None
  }
}

object Matrix {

  def nullMatrix(rows: Int, columns: Int): Matrix[Double] =
    Matrix(IndexedSeq.tabulate(rows) { _ =>
      IndexedSeq.tabulate(columns) { _ => 0.0 }
    })

  def apply(data: String): Either[IllegalArgumentException, Matrix[Double]] = {
    if (data.startsWith("[[[") || data.startsWith("]]]")) {
      Left(new IllegalArgumentException("Tensors of order > 2 not supported"))
    } else if (data.count(_ == '[') != data.count(_ == ']')) {
      Left(new IllegalArgumentException("Unbalanced square brackets"))
    } else {
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
    }
  }

  def fromString(data: String): Matrix[Double] = LinearAlgebra.getSafe(Matrix(data))

  def identityMatrix(order: Int): Matrix[Double] =
    Matrix(IndexedSeq.tabulate(order) { row =>
      IndexedSeq.tabulate(order) { column =>
        if (row == column) 1.0 else 0.0
      }
    })
}