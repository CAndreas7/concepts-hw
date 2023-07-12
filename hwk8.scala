object hwk8 {
	def main(args: Array[String]): Unit = {
		val v1 = List(1, 2, 3)
		val v2 = List(4, 5, 6)
		val s = 2
		val m1 = List(List(1, 2, 3), List(1, 1, 1))
		val m2 = List(List(1, 1), List(2, 1), List(3, 1))

		println(vectorAdd(v1, v2))
		println(svProduct(s, v1))
		println(vmProduct(v1, m1))
		println(vmProduct(List(1,2,3), List(List(1,1), List(2,1), List(3,1))))
		println(matrixProduct(m1, m2))
	}
  def vectorAdd(v1: List[Int], v2: List[Int]): List[Int] = {
    (v1, v2).zipped.map(_ + _).toList
  }
  def svProduct(s: Int, v: List[Int]): List[Int] = {
    v.map(_ * s)
  }
  def vmProduct(v: List[Int], m: List[List[Int]]): List[Int] = {
    m.transpose.map(row => (v, row).zipped.map(_ * _).sum)
  }
  def matrixProduct(m1: List[List[Int]], m2: List[List[Int]]): List[List[Int]] = {
    m1.map(row => vmProduct(row, m2))
  }
}