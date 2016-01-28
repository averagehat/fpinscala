
import java.lang.{reflect => jreflect}
import scala.reflect.mirror._

/**
  * Scala counterpart of java.lang.reflect.InvocationHandler
  */
trait InvocationHandler {
  def invoke(proxy: AnyRef, method: Symbol, args: Array[AnyRef]): AnyRef
}

object ScalaProxy {
  def apply[T <: AnyRef](handler: InvocationHandler)(implicit manifest: Manifest[T]): T = {
    val clazz = manifest.erasure
    val h = new ScalaHandler(handler)
    jreflect.Proxy.newProxyInstance(clazz.getClassLoader(), Array(clazz), h).asInstanceOf[T]
  }
  private class ScalaHandler(handler: InvocationHandler) extends jreflect.InvocationHandler {
    def invoke(proxy: Object, method: jreflect.Method, args: Array[Object]): Object = {
      val m = methodToSymbol(method)
      handler.invoke(proxy, m, if (args != null) args else Array.empty)
    }
    /**
      * Maps java.lang.reflect.Method to scala.reflect.api.Symbols.Symbol
      * corresponding to that method.
      *
      * TODO: Improved (complete) implementation of this method should
              be in Mirror class similarly to classToType method defined there.
      */
    private def methodToSymbol(m: jreflect.Method): Symbol = {
      val ClassInfoType(_, decls, _) = classToSymbol(m.getDeclaringClass).info
      val jname = m.getName
      //TODO: handle overloaded defs
      decls.find(_.encodedName == jname).get
    }
  }
}

trait Foo {
  def foo(x: Int, y: String): Unit
  def bar_!(): Unit
}

/**
  * Test of ScalaProxy. Run using trunk version of compiler.
  * Type
  * scalac Proxy.scala && scala test.Test
  */
object Test extends App {
  val h = new InvocationHandler {
    def invoke(proxy: AnyRef, m: Symbol, args: Array[AnyRef]): AnyRef = {
      //TODO: if method has multiple argument list resultType will be MethodType again
      //so we need recursive extraction below
      val MethodType(params, resultType) = m.info
      val paramsStr = {
        val names = params.map(_.name.toString)
        val types = params.map(_.info.toString)
        ((names zip args zip types) map { case ((n, a), t) => n + ": " + t + " = " + a }).mkString("(", ", ", ")")
      }
      println("called " + m.decodedName + paramsStr + ": " + resultType)
      null
    }
  }
  val p = ScalaProxy[Foo](h)
  p.foo(1, "str")
  p.bar_!()
  //prints:
  //called foo(x: Int = 1, y: String = str): Unit
  //called bar_!(): Unit
}