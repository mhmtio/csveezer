package io.terrafino.csveezer

import scala.language.reflectiveCalls
import java.io._
import scala.collection.mutable.ArrayBuffer

object Util {

  def using[A <: { def close(): Unit }, B](resource: A)(f:A => B):B =
    try {
      f(resource)
    } finally {
      resource.close()
    }

  def readTextFile(f: String): Vector[String] = {
    try {
      val lines = using(scala.io.Source.fromFile(f)) {
        source => (for (line <- source.getLines) yield line).toList
      }
      lines.toVector
    } catch {
      case e: Exception => { println("Could not read file!"); Vector() }
    }
  }

  def saveToFile(f: String, lines: ArrayBuffer[String]): Unit = {
    val file = new File(f)
    val bw = new BufferedWriter(new FileWriter(file))
    lines.foreach(bw.write(_))
    bw.close 
  }

}

