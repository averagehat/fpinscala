package fpinscala.laziness

import scala.{Option => _, Some => _, Either => _, _}
import scalaz.{\/-, -\/, \/}
/**
  * Created by michael panciera on 1/25/16.
  */
object Foo {

  implicit class BlingString(val string: String) extends AnyVal {
    def bling = "*" + string + "*"
  }

  implicit

  implicit class Jobbable(val fs: ) extends AnyVal {
    def %- = a => fs.map(dirname(a)/noExt(a.last))
    def %= (b: String) : Seq[Path]  = fs.map( a => dirname(a)/swapExt(a.last, b))

  val f = "foo".bling

}
class JobGraph {

  import Foo._

  val x = "foo".bling




}
