/**
 * TaxEntryProcessor.scala
 *
 * Copyright 2019 Andrew Hughes (ahughes6@buffalo.edu)
 *
 * This work is licensed under the Creative Commons
 * Attribution-NonCommercial-ShareAlike 4.0 International License.
 * To view a copy of this license, visit
 * http://creativecommons.org/licenses/by-nc-sa/4.0/.
 *
 * Submission author
 * UBIT: brmeehan
 * Person#: 50226243
 *
 * Collaborators (include UBIT name of each, comma separated):
 * UBIT:
 */
package cse250.pa0.objects

import java.io.File
import java.io.FileWriter
import java.io.BufferedWriter

import cse250.assignments.objects.TaxEntry

import scala.collection.mutable.{ArrayBuffer, ListBuffer}

object TaxEntryProcessor {

  /*--------------------------------------------------------
  SOURCE: Scala Tour
  LINK: https://docs.scala-lang.org/tour/tour-of-scala.html
  ---------------------------------------------------------*/

  def sanitizeData(filename: String): Unit = {
    // For opening files, look at Scala Cookbook File I/O Excerpt
    val inputFile = scala.io.Source.fromFile(filename)
    // Note: lines is an iterator to the file. This is only valid as long as the file is open.
    //       Ensure you do not close the file prior to finishing the file usage.
    val lines = inputFile.getLines()

    val outputFile = new BufferedWriter(new FileWriter( new File(filename + "-updated")))

    // Without the '\n' character, all output will be written as one long line.
    // Process the lines.

    for (line <- lines){
      /*--------------------------------------------------------
      SOURCE: Java Regex Documentation
      LINK: https://docs.oracle.com/javase/7/docs/api/java/util/regex/Pattern.html
      ---------------------------------------------------------*/
      val data = line.split("""(?<!(,")[^"|,]+),(?![^"]*[,]*(",))""")
      /*--------------------------------------------------------
      SOURCE: Scala Standard Library - ListBuffer
      LINK: https://www.scala-lang.org/api/current/scala/collection/mutable/ListBuffer.html
      ---------------------------------------------------------*/
      val entryList = new ArrayBuffer[String]

      if (data.length >= 20 && !data(19).isBlank){
        for (element <- data) {
          if (element.isBlank){
            entryList.addOne("_")
          } else {
            entryList.addOne(element)
          }
        }
      }

      // Pads end of list if end of line is blank
      if (entryList.size < 46 && entryList.nonEmpty){
        for (_ <- entryList.size until 46){
          entryList.addOne("_")
        }
      }

      // Removes appropriate columns
      if (entryList.nonEmpty){
        val remove = Array(41, 40, 39, 33, 32, 31, 24, 22, 21, 20, 13, 12, 11, 10, 9, 8, 7, 1, 0)
        for (idx <- remove){
          entryList.remove(idx)
        }
      }

      /*--------------------------------------------------------
      SOURCE: Scala Cookbook Chapter 12
      LINK: https://buffalo.app.box.com/v/cse250-ScalaCookbookIO
      ---------------------------------------------------------*/
      // Writes kept columns to new file
      if (entryList.nonEmpty){
        for (i <- 0 until entryList.size - 1){
          if (entryList(i) == "_"){
            outputFile.write(",")
          } else {
            outputFile.write(entryList(i)+",")
          }
        }

        // Writes last column of row to file, including newline character
        if (entryList(entryList.size - 1) == "_"){
          outputFile.write("\n")
        } else {
          outputFile.write(entryList(entryList.size - 1) + "\n")
        }
      }
    }

    // Closes input and output files
    inputFile.close()
    outputFile.close()
  }

  def computeMostExpensiveEntry(filename: String): TaxEntry = {
    var mostExpensiveEntry = new TaxEntry
    mostExpensiveEntry.infoMap.addOne("total_value", "0")
    val inputFile = scala.io.Source.fromFile(filename)
    /*--------------------------------------------------------
    SOURCE: Scala Standard Library - ArrayOps - .drop()
    LINK: https://www.scala-lang.org/api/current/scala/collection/ArrayOps.html#drop(n:Int):Array[A]
    ---------------------------------------------------------*/
    val lines = inputFile.getLines().drop(1)
    val keys = Array("print_key", "front", "depth", "property_class", "property_class_description", "house_number", "street", "address", "city", "state", "zip_code", "deed_date", "land_value", "total_value", "sale_price", "year_built", "total_living_area", "overall_condition", "#_of_fireplaces", "#_of_beds", "#_of_baths", "council_district", "police_district", "neighborhood", "latitude", "longitude", "location")

    for (line <- lines){
      val entry = new TaxEntry
      /*--------------------------------------------------------
      SOURCE: Java Regex Documentation
      LINK: https://docs.oracle.com/javase/7/docs/api/java/util/regex/Pattern.html
      ---------------------------------------------------------*/
      val data = line.split("""(?<!(,")[^"]+),(?![^"]+(",))""")
      // Adds keys and values to TaxEntry object
      for (key <- keys){
        /*--------------------------------------------------------
        SOURCE: Scala Standard Library - Array
        LINK: https://www.scala-lang.org/api/current/scala/Array.html
        ---------------------------------------------------------*/
        /*--------------------------------------------------------
        SOURCE: Scala Standard Library - Map
        LINK: https://www.scala-lang.org/api/current/scala/collection/mutable/Map.html
        ---------------------------------------------------------*/
        if (keys.indexOf(key) < data.length){
          entry.infoMap.addOne(key, data(keys.indexOf(key)))
        } else {
          entry.infoMap.addOne(key, "")
        }
      }
      // Checks if iterated TaxEntry is more expensive than most expensive, updates mostExpensiveEntry if true
      if (entry.infoMap("total_value") != "" && entry.infoMap("total_value").toInt > mostExpensiveEntry.infoMap("total_value").toInt){
        mostExpensiveEntry = entry
      }
    }

    inputFile.close()
    // Returns mostExpensiveEntry
    mostExpensiveEntry
  }

  def computeOldestEntry(filename: String): TaxEntry = {
    var oldestEntry = new TaxEntry
    oldestEntry.infoMap.addOne("year_built", "9999")
    val inputFile = scala.io.Source.fromFile(filename)
    /*--------------------------------------------------------
    SOURCE: Scala Standard Library - ArrayOps - .drop()
    LINK: https://www.scala-lang.org/api/current/scala/collection/ArrayOps.html#drop(n:Int):Array[A]
    ---------------------------------------------------------*/
    val lines = inputFile.getLines().drop(1)
    val keys = Array("print_key", "front", "depth", "property_class", "property_class_description", "house_number", "street", "address", "city", "state", "zip_code", "deed_date", "land_value", "total_value", "sale_price", "year_built", "total_living_area", "overall_condition", "#_of_fireplaces", "#_of_beds", "#_of_baths", "council_district", "police_district", "neighborhood", "latitude", "longitude", "location")

    for (line <- lines){
      val entry = new TaxEntry
      /*--------------------------------------------------------
      SOURCE: Rexegg Regex Tutorial
      LINK: https://www.rexegg.com/
      ---------------------------------------------------------*/
      val data = line.split("""(?<!(,")[^"]+),(?![^"]+(",))""")
      for (key <- keys){
        /*--------------------------------------------------------
        SOURCE: Scala Standard Library - Array
        LINK: https://www.scala-lang.org/api/current/scala/Array.html
        ---------------------------------------------------------*/
        /*--------------------------------------------------------
        SOURCE: Scala Standard Library - Map
        LINK: https://www.scala-lang.org/api/current/scala/collection/mutable/Map.html
        ---------------------------------------------------------*/
        if (keys.indexOf(key) < data.length){
          entry.infoMap.addOne(key, data(keys.indexOf(key)))
        } else {
          entry.infoMap.addOne(key, "")
        }
      }
      // Checks if iterated TaxEntry is older than oldest, updates oldestEntry if true
      if (entry.infoMap("year_built") != "" && entry.infoMap("year_built").toInt > 1500 && entry.infoMap("year_built").toInt < oldestEntry.infoMap("year_built").toInt){
        oldestEntry = entry
      }
    }

    inputFile.close()
    // Returns oldestEntry
    oldestEntry
  }
}
