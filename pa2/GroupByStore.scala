/**
 * GroupByStore.scala
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
package cse250.pa2

import cse250.objects.{DNode, TaxEntry}

import scala.collection.mutable.ArrayBuffer
import scala.util.control.Breaks

class GroupByStore {
  // Feel free to change the default value of groupings and modify it to val/var.
  private var groupings: ArrayBuffer[DNode[TaxEntry]] = new ArrayBuffer[DNode[TaxEntry]]
  private var groupingAttribute = "STREET"
  private var numStored = 0

  /** Inserts element to head of corresponding grouping list. */
  def insert(taxEntry: TaxEntry): Unit = {
    val node = new DNode[TaxEntry](taxEntry, null, null)
    val attribute = taxEntry.infoMap(groupingAttribute)
    if (groupings.isEmpty){
      groupings.addOne(node)
    } else {
      //  https://www.scala-lang.org/api/current/scala/util/control/Breaks.html
      import Breaks.{break, breakable}
      breakable{
        for (i <- groupings.indices){
          val testAttribute = groupings(i).value.infoMap(groupingAttribute)
          if (attribute == testAttribute){
            node.next = groupings(i)
            groupings(i).prev = node
            //  https://www.scala-lang.org/api/current/scala/collection/mutable/ArrayBuffer.html
            groupings.update(i, node)
            break()
            //  https://piazza.com/class/jzk0oj3fv0q39w?cid=559
          } else if(testAttribute > attribute){
            //  https://www.scala-lang.org/api/current/scala/collection/mutable/ArrayBuffer.html
            groupings.insert(i, node)
            break()
          }
        }
        groupings.addOne(node)
      }
    }
    numStored = numStored + 1
  }

  /** Regroup . */
  def regroup(attribute: String): Unit = {
    if (attribute != groupingAttribute){
      groupingAttribute = attribute
      val oldGroupings = groupings
      groupings = new ArrayBuffer[DNode[TaxEntry]](groupings.size)
      for (i <- oldGroupings.indices){
        var current = oldGroupings(i)
        while (current != null){
          insert(current.value)
          current = current.next
        }
      }
    }
  }

  /** Returns an Iterator to all entries that can be used only once. */
  def iterator: Iterator[TaxEntry] = new Iterator[TaxEntry] {
    var current : DNode[TaxEntry] = _
    var idx = 0
    if (numStored > 0){
      current = groupings(0)
    }

    override def hasNext: Boolean = current != null

    override def next(): TaxEntry = {
      val retVal = current.value
      if (current.next != null){
        current = current.next
      } else {
        if (idx < groupings.length - 1){
          idx += 1
          current = groupings(idx)
        } else {
          current = null
        }
      }
      retVal
    }
  }
  
  /** Returns an Iterator to only the entries with matching values on the grouping attribute that can be used only once. */
  //  https://buffalo.app.box.com/v/cse250-examples/file/533661152162
  def iterator(value: String): Iterator[TaxEntry] = new Iterator[TaxEntry] {
    private var current : DNode[TaxEntry] = _
    // https://www.scala-lang.org/api/current/scala/util/control/Breaks.html
    import Breaks.{breakable, break}
    breakable{
      for (i <- groupings.indices){
        if (groupings(i).value.infoMap(groupingAttribute) == value){
          current = groupings(i)
          break()
        }
      }
    }

    override def hasNext: Boolean = current != null

    override def next(): TaxEntry = {
      val retVal = current.value
      current = current.next
      retVal
    }
  }

  def length: Int = numStored

  override def toString: String = if (numStored == 0) "" else this.iterator.addString(new StringBuilder, "\n").result()
}
