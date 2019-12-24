/**
 * DataEntryStore.scala
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
package cse250.pa1

import cse250.objects.EmbeddedListNode

import scala.util.control.Breaks

class DataEntryStore[A >: Null <: AnyRef](private val capacity: Int = 100)
  extends collection.mutable.Seq[A] {
  private val dataArray = Array.fill[EmbeddedListNode[A]](capacity)(new EmbeddedListNode[A])
  private var headIndex = -1
  private var tailIndex = -1
  private var numStored = 0

  /** Inserts element to tail of list. */
  def insert(elem: A): Unit = {
    var idx = -1
    if (numStored < capacity){
      // https://www.scala-lang.org/api/current/scala/util/control/Breaks.html
      val loop = new Breaks
      import loop.{break, breakable}
      breakable{
        for (i <- dataArray.indices){
          if (dataArray(i).value == null){
            idx = i
            break()
          }
        }
      }
      if (numStored == 0){
        headIndex = idx
        tailIndex = idx
      } else {
        dataArray(idx).prev = tailIndex
        dataArray(tailIndex).next = idx
      }
      numStored = numStored + 1
    } else {
      idx = headIndex
      headIndex = dataArray(idx).next
      dataArray(headIndex).prev = -1
      dataArray(idx).prev = tailIndex
      dataArray(tailIndex).next = idx
    }
    dataArray(idx).value = elem
    dataArray(idx).next = -1
    tailIndex = idx
  }

  /** Removes all copies of the given element. */
  def remove(elem: A): Boolean = {
    var removed = false
    for (idx <- dataArray.indices){
      if (dataArray(idx).value == elem){
        if (idx == headIndex){headIndex = dataArray(headIndex).next}
        if (idx == tailIndex){tailIndex = dataArray(tailIndex).prev}
        dataArray(idx).value = null
        val remPrev = dataArray(idx).prev
        val remNext = dataArray(idx).next
        dataArray(idx).prev = -1
        dataArray(idx).next = -1
        if (remPrev != -1){dataArray(remPrev).next = remNext}
        if (remNext != -1){dataArray(remNext).prev = remPrev}
        numStored = numStored - 1
        if (numStored == 0){
          headIndex = -1
          tailIndex = -1
        }
        removed = true
      }
    }
    removed
  }

  /** Returns the count of nodes containing given entry. */
  def countEntry(entry: A): Int = {
    var count = 0
    for (elem <- this){
      if (elem == entry){
        count = count + 1
      }
    }
    count
  }

  /** Gets the element at the specified index. */
  override def apply(idx: Int): A = {
    require(0 <= idx && idx < numStored)
    var current = dataArray(headIndex)
    for (_ <- 0 until idx){
      current = dataArray(current.next)
    }
    current.value
  }

  /** Replaces element at given index with a new value. */
  override def update(idx: Int, elem: A): Unit = {
    require(0 <= idx && idx <= numStored)
    if (idx == numStored){
      this.insert(elem)
    } else {
      var current = headIndex
      var count = 0
      while (count < idx){
        count += 1
        current = dataArray(current).next
      }
      
    }
  }

  /** Returns an Iterator that can be used only once. */
  def iterator: Iterator[A] = new Iterator[A] {
    private var current = headIndex

    override def hasNext: Boolean = current != -1

    override def next(): A = {
      val prev = current
      current = dataArray(current).next
      dataArray(prev).value
    }
  }

  /** Returns the length of the stored list. */
  override def length: Int = numStored

  override def toString: String = if (numStored == 0) "" else this.iterator.addString(new StringBuilder, "\n").result()
}
