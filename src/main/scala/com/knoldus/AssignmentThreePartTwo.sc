//Find the last element of list with its index value(dont use inbuilt methods to extract last element directly)
val list= List(1,2,3,4,5,6,7,8,9)
val listWithIndex = list.zipWithIndex
val lastElement = listWithIndex.foldLeft((0,0)){(_, elem)=> elem}

val lastElementNew = for(index <- list.indices
                      if index == list.length-1
                         )yield(list(index),index)


//print the table of each element in the List

val table = for(number <- list ; multiplyFactor <- 1 to 10)yield number*multiplyFactor

//aggregate the contents of two lists of same size into a single list
//	List(1,2) and List("a", "b") results List(List(1, "a"), List(2, "b"))

val listInt = List(1,2,3)
val listString = List("a","b","c")
val listMix = listInt zip listString

val listMix2 = for( a <- listInt.indices )yield List(listInt(a), listString(a))
listMix2.toList

//find sum and multiplication of the list (dont use inbuilt methods)
list.foldLeft(0){(acc,elem)=> acc + elem}

list.foldLeft(1){(acc,elem)=>acc * elem}

//Implement Stack and Queue using Lists.

val stackThroughList = list :+ 10

val queueThroughList = 0 :: list