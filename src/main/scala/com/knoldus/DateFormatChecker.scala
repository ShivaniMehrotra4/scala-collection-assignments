package com.knoldus

object DateFormatChecker extends  App {
  def apply(dd : Int , mm : Int , yyyy : Int , HH :Int , MM : Int , SS : Int): String =
  {
    dd + "-" + mm + "-" + yyyy + " " + "HH" + ":" + MM + ":" +SS
  }
  def unapply(str : String) : Option[String] = {
    val parts = str.split(" ")
    if(parts.length==2) {
      val subParts = parts(0).split(("-"))
      if (subParts.length == 3) {
        Some(subParts(0))
      }
      else
        {
          Some("No Valid Day")
        }
    }
    else
      None
  }
  println(unapply("04-03-1996 10:09:56"))
}
