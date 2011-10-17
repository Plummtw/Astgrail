package org.plummtw.astgrail.enum

object UserFlagEnum extends Enumeration {
  type UserFlagEnum = Value

  val UNAUTHORIZED    = Value("UA")  
  implicit def userflagenum2String (en : UserFlagEnum.Value) : String = en.toString
}

object UserLoginEnum extends Enumeration {
  type UserLoginEnum = Value

  val LOGIN             = Value("I")  // 登入
  val LOGOUT           = Value("O")  // 登出
  val LOGIN_FAIL        = Value("F")  // 登入失敗

  implicit def userloginenum2String (en : UserLoginEnum.Value) : String = en.toString
}

