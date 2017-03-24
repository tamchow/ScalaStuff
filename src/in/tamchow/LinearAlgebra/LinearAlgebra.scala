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

  def getSafe[A](action: => Either[Throwable, A]): A = {
    action match {
      case Left(exception) => throw exception
      case Right(data) => data
    }
  }
}

case class Matrix[T](private val data: IndexedSeq[IndexedSeq[T]])(implicit numeric: Numeric[T]) {

  import LinearAlgebra.getSafe
  import numeric._

  lazy val (rows, columns) = (data.length, data.headOption.getOrElse(IndexedSeq()).length)

  override lazy val toString: String = data map {
    row => row mkString("[", ", ", "]")
  } mkString(s"${getClass.getSimpleName}($rows, $columns) = [", ", ", "]")

  lazy val toPrintableString: String = data map {
    row => row mkString("[", ", ", "]")
  } mkString("", "\n", "\n")

  /**
    * Accesses an element at (i, j)
    *
    * @param i row index, 1 - based
    * @param j column index, 1 - based
    * @return the element @ (i, j)
    */
  def apply(i: Int, j: Int): Either[IndexOutOfBoundsException, T] = checkBoundsException(data(i)(j))(i, j)

  def get(i: Int, j: Int): T = getSafe(this (i, j))

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

  def +(matrix: Matrix[T]): Matrix[T] = {
    Matrix(for ((ourRow, theirRow) <- data.zip(matrix.data)) yield {
      for ((ourElement, theirElement) <- ourRow.zip(theirRow)) yield {
        theirElement + ourElement
      }
    })
  }

  def unary_- = Matrix(data.map(row => row.map(element => -element)))

  def -(matrix: Matrix[T]): Matrix[T] = this + (-matrix)

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

  def *(matrix: Matrix[T]): Matrix[T] = getSafe(product(matrix))

  lazy val transpose: Matrix[T] = {
    Matrix(IndexedSeq.tabulate(columns) { row =>
      IndexedSeq.tabulate(rows) {
        column => data(column)(row)
      }
    })
  }

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
          // splitted into rows
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