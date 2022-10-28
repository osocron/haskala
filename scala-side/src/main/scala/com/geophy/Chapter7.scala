package com.geophy

class User(val name: String, val lastName: String)

object User {

  def unapply(user: User): Option[(String, String)] = 
    if (user.lastName == "Baggins") Some((user.name, user.lastName))
    else None

}

object Chapter7 extends App:

  def q1(num: Int): Int =
    val num = 10
    num + 30

  val a1 = q1(20)
  val unnaplied: Function1[Int, Int] = q1

  val myUser = User("Bilbo", "Something Else")
  val myOtherUser = User("Bilbo", "Baggins")
  println(myUser.toString)
  println(myOtherUser.toString)
  println(myUser == myOtherUser)

  myUser match {
    case User(name, lastName) => println("You are a Hobbit!")
    case _ => println("Please don't be a wraith 🙏")
  }

  val myLabda = (x: Int) => (y: Int) => x + y
  val partiallyApplied = myLabda(1)


  def myFun(x: Int)(y: Int): Int = x + y
  val partiallyApplied2 = myFun(1)

  class MyLabda(y: Int) extends Function1[Int, Int]:
    override def apply(x: Int): Int = x + y

  val result = new MyLabda(10).apply(30)

  List(1,2,3).map(x => x + 10)

  println(a1)