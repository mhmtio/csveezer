package io.terrafino.csveezer

import io.terrafino.csveezer.Util._
import scala.collection.mutable.ArrayBuffer

class Mapping(val line: String) {
  val cols = line.split(",")
  val regex = cols(0).r
  val ptype = cols(1)
  val descr = cols(2)
}

class Entry(val mappings: Vector[Mapping], val line: String) {
  val cols = line.split(",")

  val pattern = "(..)/(..)/(....)".r
  val pattern(day, month, year) = cols(1)
  val origDate = cols(1)
  val date = s"${year}${month}${day}".toInt
  val amount = cols(3).toDouble
  val category = cols(4)
  val comment = cols(5)

  def getInfo():(String, String) = {
    for (mapping <- mappings) {
      mapping.regex.findFirstIn(comment) match {
        case Some(_) => return (mapping.ptype, mapping.descr)
        case None    => 
      } 
    }
    (comment, comment)
  }

  def getInfo2(m: Vector[Mapping]): (String, String) = {
    if (m.size == 0) (comment, comment)
    else {
      val first = m(0)
      first.regex.findFirstIn(comment) match {
        case Some(_) => (first.ptype, first.descr)
        case None    => getInfo2(m.tail)
      }
    }
  }

  var (ptype, descr) = getInfo2(mappings)

  override def toString(): String = s"$origDate,$amount,$ptype,$descr"

  def getDate() = date
  def getLine(balance: Double, buffer: ArrayBuffer[String]) = {
    val newBalance = balance+amount
    buffer.append("%s,%.2f,%.2f,%s,%s\n".format(origDate,amount,newBalance,ptype,descr))
    newBalance
  }
    
}

object CSVeezer extends App {

  val config = readTextFile(args(0));
  val mappings = config.map(new Mapping(_)) 

  val rows = readTextFile(args(1)).filterNot(_.startsWith("Number,"))
  val entries = rows.map(new Entry(mappings, _)).sortBy(_.getDate())
  var b: Double = args(2).toDouble
  var l: String = ""
  var lines:ArrayBuffer[String] = new ArrayBuffer()
  entries.foreach {
    e => {b = e.getLine(b, lines)}
  }
  saveToFile(args(3), lines)
}
