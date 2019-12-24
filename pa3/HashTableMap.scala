/**
 * cse250.pa5.HashTableMap.scala
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
package cse250.pa5

import scala.collection.mutable.ListBuffer
import scala.util.hashing.Hashing

class HashTableMap[K, V]()(implicit hash: Hashing[K]) extends cse250.objects.Map[K, V] {
  var n = 0
  var N = 10
  var alpha: Double = 0.0
  val alphaMax: Double = 0.6

  var bucketArray: Array[ListBuffer[(K, V)]] = Array.fill[ListBuffer[(K,V)]](N)(ListBuffer[(K,V)]())

  def rehash(newSize: Int): Unit = {
    println("rehash")
    if (newSize > N) {
      val oldBucketArray = bucketArray
      n = 0
      N = newSize
      bucketArray = Array.fill[ListBuffer[(K,V)]](N)(ListBuffer[(K,V)]())
      alpha = n / N
      for (bucket <- oldBucketArray; elem <- bucket) addOne(elem)
    }
  }

  override def addOne(elem: (K, V)): Unit = {
    //println(elem._1 + " => " + elem._2)
    val index = hash.hash(elem._1) % N
    val chain = bucketArray(index)

    var found = false
    for (keyVal <- chain; i <- chain.indices if !found){
      if (keyVal._1 == elem._1){
        found = true
        chain.update(i, elem)
      }
    }
    if (!found){
      chain.prepend(elem)
      n += 1
      alpha = n.toDouble / N.toDouble
      //println(alpha + " = " + n + " / " + N)
      if (alpha > alphaMax) rehash(N * 2)
    }
    //println("\n\n")

  }

  override def get(key: K): Option[V] = {
    val lookupIndex = hash.hash(key) % N
    for (elem <- bucketArray(lookupIndex)) {
      if (elem._1 == key) return Some(elem._2)
    }
    None
  }

  override def iterator: Iterator[(K, V)] = new Iterator[(K, V)] {

    val iterArray = new Array[(K, V)](n)
    var idx = 0

    for (bucket <- bucketArray){
      for (elem <- bucket){
        iterArray(idx) = elem
        idx += 1
      }
    }
    idx = 0

    override def hasNext: Boolean = idx < n

    override def next(): (K, V) = {
      val retVal = iterArray(idx)
      idx += 1
      retVal
    }
  }
}















