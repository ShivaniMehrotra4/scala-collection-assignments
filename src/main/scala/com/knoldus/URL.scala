package com.knoldus

object URL extends App {
  def apply(protocol:String,domain:String,path:String,params: Map[String, String]): String = {

    protocol + "://" + domain + "/" + path + "?" + params

  }
  def unapply(url:String):Option[(String , String, Map[String,String])] = {

    @scala.annotation.tailrec
    def checkEvenOdd(mapFinal: Map[String, String], arr: Array[String]): Map[String, String] = {
      if (arr.length > 1) {
        checkEvenOdd(mapFinal ++ Map(arr(0) -> arr(1)), arr.slice(2, arr.length))
      }
      else
        mapFinal
    }
    val v1 = url.split("://")
    if (v1.length > 1) {
      val v2 = v1(1).split("(?<=com)")
      val v3 = v2(1).split("[?]")
      val v4: Array[String] = v3(1).split("[=\\&]")
      val checkResult = checkEvenOdd(Map.empty[String, String], v4)
      Some(v1(0),v2(0),v3(0),checkResult)
    }
      else None
    }
  println(unapply("https://aws.amazon.com/console/home?state=hash&isauthcode=true&code=112"))

}

