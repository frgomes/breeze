package breeze.frames.util


object RichSeqInt extends RichSeqInt
trait RichSeqInt {
  import scala.collection.immutable.TreeSet

  implicit class RichSeqInt(seq: Seq[Int]) {
    def toTreeSet: TreeSet[Int] =
      (TreeSet.empty[Int] /: seq) {
        (acc, item) =>
          acc + item
      }
  }
}

object RichVector extends RichVector
trait RichVector {
  import scala.reflect.ClassTag
  import breeze.linalg.DenseVector

  implicit class RichVector(tensor: DenseVector[String]) {
    def indexOf(value: String): Seq[Int] =
      tensor.mapPairs({case (k,v) => if(v == value) Some(k) else None}).toArray.flatten
    def as[@specialized(Int, Double, Long, Float) T : ClassTag] : DenseVector[T] =
      for((index, data) <- tensor.pairs if(data.isInstanceOf[T])) yield({ data.toDouble.asInstanceOf[T] })
  }
}


  /*
  object any2Double extends Function1[Any,Double] {
    override def apply(any: Any): Double =
      any match { case i: Int => i case f: Float => f case d: Double => d }
  }

  object any2LocalDateExcel extends Function1[Any,LocalDate] {
    override def apply(any: Any): LocalDate =
      LocalDate.ofEpochDay(0).plusDays(any2Double(any).intValue - 25569)
  }

  object any2DateExcel extends Function1[Any,Date] {
    override def apply(any: Any): Date =
      Date.from(any2LocalDateExcel(any).atStartOfDay(ZoneId.systemDefault()).toInstant())
  }
  */
