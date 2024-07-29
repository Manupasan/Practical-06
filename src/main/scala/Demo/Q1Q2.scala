import scala.collection.mutable.Map

case class Product(name: String, quantity: Int, price: Double)

val inventory1: Map[Int, Product] =
  Map(
    101 -> Product("apple", 25, 100.00),
    102 -> Product("banana", 10, 15.00),
    103 -> Product("mango", 25, 70.00),
  )

val inventory2: Map[Int, Product] =
  Map(
    101 -> Product("apple", 25, 100.00),
    105 -> Product("Guava", 40, 50.00),
    106 -> Product("papaya", 10, 200.00)
  )

var stdtuple = ()

@main def main() =
  println("========== Retrieve All Product Names ==========")
  println("Product names :" + retrieveAllProductNames(inventory1))

  println("\n========== Total Value of all Products in Inventory1 ==========")
  println("Total value of all products in inventory1 : " + totalValue(inventory1))

  println("\n========== IsEmpty Inventory1 ==========")
  isEmptyInventory(inventory1)

  println("\n========== Merged Inventory ==========")
  println("Merged Inventory: " + mergeInventories(inventory1, inventory2))

  println("\n========== Check a Product ==========")
  print("Enter a product ID : ")
  val item = scala.io.StdIn.readInt()
  checkProduct(inventory1,item)

  getStudentInfo()




def retrieveAllProductNames(inventory: Map[Int, Product]): List[String] =
  inventory.values.map(_.name).toList

def totalValue(inventory: Map[Int, Product]): Double =
  inventory.values.map(product => product.quantity * product.price).sum

def isEmptyInventory(inventory: Map[Int, Product]): Unit =
  if(inventory.isEmpty)
    println("Inventory is empty!")
  else
    println("Inventory is not empty!")

def mergeInventories(inventory1: Map[Int, Product], inventory2: Map[Int, Product]) =
  (inventory1.keySet ++ inventory2.keySet).map( key => {
    val product1 = inventory1.getOrElse(key, Product("", 0, 0.0))
    val product2 = inventory2.getOrElse(key, Product("", 0, 0.0))

    val name = if (product1.name.nonEmpty) product1.name else product2.name
    val quantity = product1.quantity + product2.quantity
    val price = Math.max(product1.price, product2.price)

    key -> Product(name, quantity, price)
  }).toMap

def checkProduct(inventory: Map[Int, Product], item: Int) =
  if(inventory.contains(item))
    val prdct = inventory(item)
    println(s"Product exists!  $prdct")

  else
    println("Product does not exists!")

def getStudentInfo() =
  println("\n========== Enter Student Details ==========")

  val studentTup = getStudentInfoWithRetry()
  val percent = (studentTup._2.toDouble/studentTup._3)*100
  var grade = ""
  percent match
    case x if x >= 90 => grade = "A"
    case x if x >= 75  && x < 90 => grade = "B"
    case x if x >= 50 && x < 75 => grade = "C"
    case x if x < 50 => grade = "D"

  printStudentRecord(studentTup ++ (percent, grade))

def printStudentRecord(tuple: (String, Int, Int, Double, String) ) =
  println("\n========== Print Student Record ==========")

  println("Name : " + tuple._1)
  println("Marks : "+ tuple._2)
  println("Total Marks : " + tuple._3)
  println("Percentage : " + tuple._4)
  println("Grade : " + tuple._5)

def validateInput(tuple: (String, Int, Int)) : (Boolean,Option[String]) =
  if(tuple._1 == "")
    (false, Some("!!!!!Name cannot be empty!!!!!\n"))
  else if(tuple._2 < 0 || tuple._2 > tuple._3)
    (false, Some("!!!!!Marks are positive integers and not exceeding the total possible marks!!!!!\n"))
  else
    (true, None)

def getStudentInfoWithRetry() : (String, Int, Int) =
  val name = scala.io.StdIn.readLine("Enter student name : ")
  print("Enter marks : ")
  val marks = scala.io.StdIn.readInt()
  print("Enter total marks: ")
  val totalmarks = scala.io.StdIn.readInt()

  val (isValid, errorMessage) = validateInput(name, marks, totalmarks)
  if(isValid)
    (name, marks, totalmarks)
  else
    println(errorMessage.get)
    getStudentInfoWithRetry()
