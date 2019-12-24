val node = new DNode[TaxEntry](taxEntry, null, null)
var idx = -1
// https://www.scala-lang.org/api/current/scala/util/control/Breaks.html
val loop = new Breaks
import loop.{break, breakable}
breakable{
  for (i <- groupings.indices){
    if (groupings(i).value.infoMap(groupingAttribute) == taxEntry.infoMap(groupingAttribute)){
      idx = i
      break()
    } else if (groupings(i).value.infoMap(groupingAttribute) < taxEntry.infoMap(groupingAttribute)){
      idx = i
      groupings.addOne(groupings(groupings.size - 1))
      for (j <- groupings.size - 2 to i by -1){
        groupings.insert(j, groupings(j - 1))
      }
      break()
    }
  }
}

if (idx != -1){
  node.next = groupings(idx)
  groupings(idx).prev = node
  groupings.insert(idx, node)
} else {
  groupings.addOne(node)
}
numStored += 1



//  https://docs.oracle.com/javase/8/docs/api/java/lang/String.html
val compareLast = groupings(groupings.length - 1).value.infoMap(groupingAttribute).compareTo(taxEntry.infoMap(groupingAttribute))





def insert(taxEntry: TaxEntry): Unit = {
  val node = new DNode[TaxEntry](taxEntry, null, null)
  if (numStored == 0){
    groupings.addOne(node)
  } else {
    //  https://docs.oracle.com/javase/8/docs/api/java/lang/String.html
    val compareLast = groupings(groupings.length - 1).value.infoMap(groupingAttribute).compareTo(taxEntry.infoMap(groupingAttribute))
    if (compareLast < 0){
      groupings.addOne(node)
    } else {
      //  https://www.scala-lang.org/api/current/scala/util/control/Breaks.html
      import Breaks.{break, breakable}
      breakable{
        for (i <- groupings.indices){
          val compare = groupings(i).value.infoMap(groupingAttribute).compareTo(taxEntry.infoMap(groupingAttribute))
          if (compare == 0){
            node.next = groupings(i)
            groupings(i) = node
            break()
          } else if(compare < 0){
            groupings.addOne(groupings(groupings.length - 1))
            for (j <- groupings.length - 1 until i by -1){
              groupings(j) = groupings(j-1)
            }
            groupings(i) = node
            break()
          }
        }
      }
    }
  }
  numStored += 1
}






val node = new DNode[TaxEntry](taxEntry, null, null)
if (groupings.isEmpty){
  groupings.addOne(node)
} else {

  //  https://docs.oracle.com/javase/8/docs/api/java/lang/String.html
  val compareLast = groupings(groupings.length - 1).value.infoMap(groupingAttribute).compareTo(taxEntry.infoMap(groupingAttribute))
  if (compareLast > 0){

    groupings.addOne(node)

  } else {

    //  https://www.scala-lang.org/api/current/scala/util/control/Breaks.html
    import Breaks.{break, breakable}
    breakable{
      for (i <- groupings.indices){
        val compare = groupings(i).value.infoMap(groupingAttribute).compareTo(taxEntry.infoMap(groupingAttribute))
        if (compare == 0){

          node.next = groupings(i)
          groupings(i).prev = node
          groupings.insert(i, node)
          break()

        } else if(compare > 0){

          for (j <- groupings.length until i by -1){
            groupings.insert(j, groupings(j - 1))
          }
          groupings.insert(i, node)
          break()

        }
      }
    }
  }
}
numStored += 1