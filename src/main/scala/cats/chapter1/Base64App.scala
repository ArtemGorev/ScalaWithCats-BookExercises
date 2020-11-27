package cats.chapter1

import java.util.Base64

object Base64App extends App {
  val x = "n4bQgYhMfWWaL-qgxVrQFaO_TxsrC4Is0V1sFbDwCgg="
  val y = Base64.getEncoder.encodeToString(Base64.getUrlDecoder.decode(x))
  println(y)
  val z = Base64.getDecoder.decode(y)
  println(z)

  val zz = "n4bQgYhMfWWaL-qgxVrQFaO_TxsrC4Is0V1sFbDwCgg="
//  val zz = "n4bQgYhMfWWaL-qgxVrQFaO_TxsrC4Is0V1sFbDwCgg="

}
