
object Hofs extends App {
 // i have implemented the solution of each method in both ways
  val Numbers = List(1, 2, 3, 4, 5)

  //Fold
  def sum(list: List[Int]): Int =
    list match {
      case Nil => 0
      case head :: tail => head + sum(tail)
    }
  //Printing the result
  println(sum(Numbers))

  val SumOfList = Numbers.fold(0)((x, y) => x + y)
  //Printing the result
  println(SumOfList)

/**********************************/

  //Map
  def squareAll( list:List[Int] ): List[Int] =
    list match {
      case Nil => Nil
      case head :: tail => (head*head) :: squareAll(tail)
    }
  //Printing the result
  println(squareAll(Numbers))

  def map( f:(Int) => Int, list:List[Int] ): List[Int] =
    list match {
      case Nil => Nil
      case head :: tail => f(head) :: map(f,tail)
    }
  //Printing the result
  println(squareAll(Numbers))
  map((x:Int) => x * x, Numbers)

  /**********************************/

  //Filter
  // filters all Even numbers in the list
  def isEven(x:Int): Boolean = x % 2 == 0
  val filterNormal = Numbers.filter(isEven)
  //Printing the result
  println(filterNormal)

  def filter( p:(Int) => Boolean, list:List[Int] ): List[Int] =
    list match {
      case Nil => Nil
      case head :: tail => if (p(head)) head :: filter(p,tail) else filter(p,tail)
    }
  val filterHof = filter(isEven, Numbers)
  println(filterHof)

  /**********************************/

  //FlatMap
  val result = Numbers.flatMap{x => List(x,x+1)}
  println(result)



  def flatMap(list: List[Int])(f: Int => List[Int]): List[Int] =
    list match {

      case Nil => Nil
      case head :: tail => list(head) ::: flatMap(tail,f)
    }
}

