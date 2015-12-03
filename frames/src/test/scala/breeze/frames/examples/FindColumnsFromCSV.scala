package breeze.frames.examples

import breeze.io.CSVReader
import breeze.linalg.{DenseVector,DenseMatrix}

import breeze.frames.util.RichSeqInt._
import breeze.frames.util.RichVector._


object Main {
  def csvRead(file: java.io.File,
              separator: Char=',',
              quote: Char='\"',
              escape: Char='\\',
              skipLines: Int = 0): DenseMatrix[String] = {
    val mat = CSVReader.read(new java.io.FileReader(file), separator, quote, escape, skipLines)
    if(mat.length == 0) {
      DenseMatrix.zeros[String](0, 0)
    } else {
      DenseMatrix.tabulate(mat.length, mat.head.length)((i,j) => mat(i)(j))
    }
  }

  val home = System.getenv("HOME")
  val path = home + "/Documents/Workspace/Models/PricesFX.csv"
  val sheet = "Prices Database"
  val currency = "EUR"
  val term = "Cash"
  val row0 = 4
  val start = "2010-01-01"

  val database : DenseMatrix[String] = csvRead(new java.io.File(path))
  val posCurr  : Int = database(0, ::).t.indexOf(currency).toTreeSet.head
  val posTerm  : Int = database(1, ::).t.indexOf(term).toTreeSet.from(posCurr).head
  val dates    : DenseVector[String] = database(::, 0).slice(row0, database.rows)
  val prices   : DenseVector[Double] = database(::, posTerm).slice(row0, database.rows).as[Double]

  val row1 = dates.indexOf(start).head


  val A=0; val B=1; val C=2; val D=3
  val size=prices.size-row1+1

  val m = DenseMatrix.zeros[Double](size, D)

  val c_ewma = 0.94

  var count = row1; var i = 0
  while(count<size) {
      //FIXME: m(i,A) = dates(i)
      m(i,B) = prices(i)
      m(i,C) = if(i==0) 0.0 else m(i,B) - m(i-1,B)
      m(i,D) = if(i==0) 0.0 else m(i,C) * m(i,C)
    count = count + 1; i = i+1;
  }

  import breeze.stats._
  val v_var = variance( m(::,C).slice(1, size) )
  val v_rss = variance( m(::,D).slice(1, size) )
}
