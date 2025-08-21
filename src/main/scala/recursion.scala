import scala.annotation.tailrec
import scala.collection.convert.ImplicitConversions.`map AsJavaMap`
import scala.collection.mutable
import scala.concurrent.Future
import scala.jdk.CollectionConverters.IteratorHasAsScala

object recursion extends App {
  println(s"-------------------TAIL RECURSION------------------")
  val list: List[Int] = List(1, 2, 3)

  @tailrec
  def lengthOfList(list: List[Int], acc: Int): Int = list match {
    case Nil => acc
    case _ :: tail => lengthOfList(tail, acc + 1)
  }

  println(s"Length of a list: ${lengthOfList(list, 0)}")

  @tailrec
  def reverseAList(list: List[Int], finalList: List[Int]): List[Int] = list match {
    case Nil => finalList
    case head :: tail => reverseAList(tail, head +: finalList)
  }

  println(s"Reverse a list: ${reverseAList(list, Nil)}")

  @tailrec
  def findMax(list: List[Int], max: Int): Int = list match {
    case Nil => max
    case head :: tail => findMax(tail, if (head > max) head else max)
  }

  println(s"Maximum element in the list: ${findMax(list, -1)}")

  @tailrec
  def checkListIsPalindrome(list: List[Int], lp: Int, rp: Int): Boolean = {
    if (lp >= rp) true
    else if (list(lp) != list(rp)) false
    else checkListIsPalindrome(list, lp + 1, rp - 1)
  }

  println(s"Palindrome check: ${checkListIsPalindrome(list, 0, lengthOfList(list, 0) - 1)}")

//  @tailrec
  def sumOfList(list: List[Int], sum: Int): Int = list match {
    case Nil => sum
    case head :: tail => sumOfList(tail, sum + head)
  }

  println(s"Sum of List: ${sumOfList(list, 0)}")

  @tailrec
  def fibonacci(a: Int, b: Int, n: Int, x: Int, list: List[Int]): List[Int] = {
    if (x == n) list
    else {
      val newA = a + b
      val newList: List[Int] = list
      fibonacci(b, newA, n, x + 1, newList :+ newA)
    }
  }

  val fibonacciSeries: List[Int] = List(1, 2)
  println(s"Fibonacci series: ${fibonacci(1, 2, 10, 2, fibonacciSeries)}")

  @tailrec
  def map(list: List[Int], f: Int => Int, finalList: List[Int]): List[Int] = list match {
    case Nil => reverseAList(finalList, Nil)
    case head :: tail => map(tail, f, f(head) :: finalList)
  }

  def f(x: Int): Int = x + 2

  val func: Int => Int = f
  println(s"Map function: ${map(list, func, Nil)}")

  val listOfString: List[String] = List("abc", "def", "ghi", "jkl")

  @tailrec
  def simulateALoop[A](list: List[A]): Unit = list match {
    case Nil =>
    case head :: tail => {
      print(s"$head ")
      simulateALoop(tail)
    }
  }
  simulateALoop(listOfString)
  println()

  @tailrec
  def flattenList(list: List[List[Int]], finalList: List[Int]): List[Int] = list match {
    case Nil => finalList
    case head :: tail =>
      print(s"${finalList ::: head} ")
      flattenList(tail,  finalList ::: head)
  }

  val nestedList: List[List[Int]] = List(List(1, 2), List(3, 4), List(5, 6))
  println(s"Flat list: ${flattenList(nestedList, Nil)}")
  println()


  println(s"-------------------SHORTHAND METHODS------------------")
  val collectMethod: List[Option[Int]] = List(Some(1), Some(2), Some(3), Some(4), None)
  println(s"Collect method : ${
    collectMethod
      .collect {
        case l => l.map(_ * 2)
      }
  }"
  )

  println(
    s"Fold method: ${
      list.foldRight(2)(
        (i, acc) => acc + i
      )
    }"
  )

  val list2: List[List[Int]] = List(List(1), List(2, 3), List(3, 1, 2), List(2, 1, 3, 1))
  val flattenList: Map[Int, Int] = list2.flatMap(x => x).groupBy(identity).mapValues(x => x.size).view.toMap
  val values: List[Int] = flattenList.flatMap(x => List(x._2)).toList.distinct.sorted
  println(s"Flatten List: $flattenList")
  println(s"Values: $values")

  val listOfMaps: List[Map[Char, Int]] = List(Map('a' -> 1, 'b' -> 2, 'c' -> 3), Map('a' -> 1, 'b' -> 2, 'c' -> 5), Map('a' -> 2, 'b' -> 4, 'c' -> 3), Map('a' -> 2, 'b' -> 4, 'c' -> 6), Map('a' -> 3, 'b' -> 6, 'c' -> 3), Map('a' -> 3, 'b' -> 6, 'c' -> 7))
  val years = listOfMaps.groupBy(f => (f('a'), f('b')))
  val yearsRange = years.map(x => x match {
    case ((x, y), group) =>
      val z = group.map(y => y('c')).distinct.sorted
      val range = z match {
        case head :: Nil => head.toString
        case head :: _ => s"$head-${z.last}"
        case _ => ""
      }
      val map = Map("Make" -> s"$x",
        "Model" -> s"$y",
        "Year" -> s"$range")
      println(map)
  })
  val list3: List[Int] = List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
  println(list3.sliding(3).toList)
  val maxInEachWindow: List[Int] = list3.sliding(3).toList.map(x => x.min)
  println(maxInEachWindow)

  val map = Map("a" -> 1, "b" -> 2)
  println(map.values.toList)

  def f2: String = {
    "Shubhang-Dutta"
  }

  println(f2)

  val list4 = List('a', 'a', 'a', 'a', 'b', 'c', 'c', 'a', 'a', 'd', 'e', 'e', 'e', 'e')
  var ansList: Vector[(Int, Char)] = Vector()
  var prev = list4.head
  println(list4)
  val finalCount = list4.tail.foldLeft((Vector.empty[(Int, Char)], 1, list4.head)) {
    case ((accList, count, prev), curr) =>
      if (prev == curr) (accList, count + 1, prev)
      else (accList :+ (count, prev), 1, curr)
  } match {
    case ((accList, count, prev)) => accList :+ (count, prev)
  }
  println(finalCount)

  val list5: List[(Int, Char)] = List((4, 'a'), (1, 'b'), (2, 'c'), (2, 'a'), (1, 'd'), (4, 'e'))
  var ansList2 = list5.foldLeft(List.empty[Char]) {
    (acc, x) =>
      x match {
        case (count, c) => acc ++ List.fill(count)(c)
      }
  }
  println(ansList2)

  val list6 = List('a', 'b', 'c', 'c', 'd')
  val n = 3
  println(list6.flatMap(x => List.fill(n)(x)))
  println(list6.splitAt(3))
  println(list6.slice(1, 4))

  def rotateList(n: Int, list: List[Int]): List[Int] = {
    val x = n % list.length
    val (prefix, suffix) = list.splitAt(x)
    suffix ++ prefix
  }

  println(rotateList(4, List(1, 2, 3, 4, 5, 6)))

  val pair = 1 -> 2
  println(pair)
  val listOfTuple = List((1, 2))
  val mapOfTuples = listOfTuple.map(tuple => Map("sellerId" -> tuple._1, "sellerOfferId" -> tuple._2))
  println(mapOfTuples)


  val option: Option[Int] = Option(3).flatMap(x => Option(4).map(y => x + y))
  println(s"Option: $option")

  val map2: Map[String, Int] = Map("a" -> 3,
    "b" -> 2,
    "c" -> 1,
    "e" -> 2,
    "d" -> 4
  )

  val sortedListOfMapEntries = map2.entrySet()
    .stream()
    .sorted((set1, set2) => set1.getValue - set2.getValue)
  var sortedMap = mutable.LinkedHashMap[String, Int]()
  sortedListOfMapEntries.iterator().asScala.foreach { entry =>
    sortedMap += (entry.getKey -> entry.getValue)
  }
  println(s"Sorted map: $sortedMap")

  val a = if (true) {
    "Hello"
  } else {
    "World"
  }

  println(s"------------------------------------------------------------>")

  @tailrec
  def factorial(n: Int, acc: Int): Int = {
    if (n == 0) return acc
    factorial(n-1, acc*n)
  }
  val factorialOf = 4
  println(s"Factorial of $factorialOf: ${factorial(factorialOf, 1)}")

  @tailrec
  def fibonacci(a: Int, b: Int, n: Int, acc: List[Int]): List[Int] = {
    if (n < n-2) return acc
    else fibonacci(b, a+b, n-1, acc :+ (a+b))
  }
  val fibnacciSeries: List[Int] = List(1, 2)
  println(s"Fibonacci series: ${fibonacci(1, 2, 10, 2, fibnacciSeries)}")

  @tailrec
  def reverseList(list: List[Int], acc: List[Int]): List[Int] = {
    if (list.isEmpty) acc
    else reverseList(list.tail, list.head +: acc)
  }
  println(s"Reversed list: ${reverseList(list, Nil)}")

  @tailrec
  def listSum(list: List[Int], acc: Int): Int = list match {
    case Nil => acc
    case head :: tail => listSum(tail, acc + head)
  }
  println(s"Sum of list: ${listSum(list, 0)}")

  @tailrec
  def checkPalindrome(s: String, lp: Int, rp: Int): Boolean = {
    if (lp >= rp) true
    else if (s.charAt(lp) != s.charAt(rp)) false
    else checkPalindrome(s, lp+1, rp-1)
  }
  val palindromeString = "race"
  println(s"Is '$palindromeString' a palindrome? ${checkPalindrome(palindromeString, 0, palindromeString.length - 1)}")

  println(s"-------------------TAIL RECURSION------------------")
  private def map2[A, B](f: A => B, list: List[A]): List[B] = {
    val resList: List[B] = Nil

    @tailrec
    def helper(list: List[A], resList: List[B]): List[B] = {
      list match {
        case Nil => resList.reverse
        case head :: tail =>
          helper(tail, f(head) :: resList)
      }
    }

    helper(list, resList)
  }
  val customMappedList: List[String] = map2[Int, String]((x: Int) => s"${x + 2}", list)
  println(s"Custom map method: $list -> $customMappedList")

  private def CustomFold[A](acc: A)(list: List[A], f: (A, A) => A): A = {

    @tailrec
    def helper(list: List[A], acc: A): A = {
      list match {
        case Nil => acc
        case head :: tail =>
          val newAcc: A = f(acc, head)
          helper(tail, newAcc)
      }
    }

    helper(list, acc)
  }
  val foldResult: Int = CustomFold[Int](0)(list, (a, b) => a + b)
  println(s"Custom fold: $foldResult")

  def prefixSum(list: List[Int]): List[Int] = {
    val resList: List[Int] = Nil

    @tailrec
    def helper(list: List[Int], resList: List[Int]): List[Int] = {
      list match {
        case Nil => resList.reverse
        case head :: tail =>
          helper(tail, if(resList.isEmpty) head :: resList else (head + resList.head) :: resList)
      }
    }

    helper(list, resList)
  }
  val prefixSumList: List[Int] = prefixSum(List(1,2,3,4,5,6))
  println(s"Prefix sum of the list: $prefixSumList")

  /*import scala.concurrent.ExecutionContext.Implicits.global
  val future = Future {
    println(Thread.currentThread().toString)
    Thread.sleep(5000)
    10
  }


  future.map(x => println(s"Future completed with value: ${x+2}"))
  println(Thread.currentThread().toString)

  Thread.sleep(8000) // let us wait for the main thread to give enough time to run and complete the future.
  */



  case class User(name: String, age: Int)
  val payload = User("Shubhang-Dutta", 21)
  println(payload.toString)

}


// Shubhang