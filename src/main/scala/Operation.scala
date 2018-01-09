class OperationFunction {
  def listMaxValue(list: List[Int], max: Int = 0): Int = {
    if (list.isEmpty) {
      max
    }
    else if (max < list.head) {
      listMaxValue(list.tail, list.head)
    }
    else {
      listMaxValue(list.tail, max)
    }
  }

  def listIteration(list: List[Int]) {
    for (listIndex <- 0 until list.size) {
      print(s"$listIndex = ${list(listIndex)} \n")
    }
  }

  def fibonacci(value: Int): Int = {
    if (value <= 1) {
      value
    }
    else {
      (fibonacci(value - 1) + fibonacci(value - 2))
    }
  }

  def digitSum(value: Int): Int = {
    val fact = factorial(value)
    val sum = digitSum(fact, 0)
    sum
  }

  def digitSum(value: Int, sum: Int): Int = {
    if (value > 0) {
      digitSum(value / 10, value % 10 + sum)
    }
    else {
      sum
    }
  }

  def factorial(factVal: Int): Int = {
    if (factVal == 1) {
      1;
    }
    else {
      factVal * factorial(factVal - 1)
    }
  }
}

object Operation {
  private val operation = new OperationFunction

  def main(args: Array[String]) {
    val number1 = 1
    val number2 = 201
    val number3 = 33
    val number4 = 62
    val number5 = 7
    operation.listIteration(List(number1, number2, number3, number4, number5))
    val maxVal = operation.listMaxValue(List(number1, number2, number3, number4, number5))
    val fabonacci = operation.fibonacci(number5 - 1)
    val digitsum = operation.digitSum(number5)
    print(maxVal + "\n")
    print(fabonacci + "\n")
    print(digitsum + "\n")
  }
}
