package com.knoldus

object URL extends App {
  def apply(protocol:String,domain:String,path:String, params:Map(String,string: String):String = {

    protocol + "://" + domain + "/" + path + "?" + params

  }
  def unapply(url:String):Option[(String , String, Map[String,String]] = {
    //val url : String = "https://aws.amazon.com/console/home?state=hash&isauthcode=true&code=112"
    val mapExample =Map()
    def checkEvenOdd(arr : Array[String]): Map[String,String] =
    {

      mapExample + Map(arr(0)->arr(1))

    }
    val parts = url.split("://")
    if(parts.length >1)
      {
        val partsInner = parts(1).split("com")
        if(partsInner.length >1)
          {
            partsInner(0) = partsInner(0) + "com"
            val partsInnerInner = partsInner(1).split("?")
            if(partsInnerInner.length >1)
              {
                  val partsInnerInnerInner : Array[String] = partsInnerInner(1).split("[&\\=]")
                  //Some(parts(0),partsInner(0),partsInnerInner(0),partsInnerInnerInner  )

              }
            else
              None
          }
        else
          None
      }
else
      None
  }

  println(unapply("https://aws.amazon.com/console/home?state=hash&isauthcode=true&code=112"))
}
