package com.knoldus

import scala.util.matching.Regex

object RegularExpression extends App {

  def checkEMail(strEmail : String): String ={
    val emailParse = new Regex("""(^[A-Za-z0-9][A-Za-z0-9]*)@((?:[A-Za-z0-9]+\.)+[A-Za-z]{2,63}$)""")
    strEmail match {
      case emailParse(user,domain) => s"user = $user and domain = $domain"
      case _=> "invalid email addresss"
    }
  }
  println(checkEMail("shivanimehrotra@gmail.com"))
}
