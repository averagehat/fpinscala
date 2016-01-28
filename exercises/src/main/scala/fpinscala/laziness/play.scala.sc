import scala.{Option => _, Some => _, Either => _, _}
import scalaz.{\/-, -\/, \/}
import scala.reflect.runtime.universe._
import scala.reflect.runtime.{currentMirror => m}
/**
  * Created by michael panciera on 1/25/16.
  */


implicit class BlingString(val string: String) extends AnyVal {
  def bling = "*" + string + "*"
}
object Foo {

  val f = "foo" //.bling

}

println(Foo.f)


val im = m reflect res.func // Instance Mirror


val apply = newTermName("apply")
val applySymbol = im.symbol.typeSignature member apply


val applyMethod = applySymbol.asMethod

val param = applyMethod.paramss(0)(0)


val name = param.name.decoded // if you want "+" instead of "$plus", for example
val type = param.typeSignature
