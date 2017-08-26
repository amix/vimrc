package testfile
import java.something.com

package object SomeObject[A <: B] extends Implicits {
  type Booger[A] = A => Unit
  type SomeType = A <: B :> C
  type SomeOtherType = A ⇒ Thing
  type Something
  type Something <: SomethingElse
  type ParserContext = Context { type PrefixType = Parser }
  
  new Something#SomethingElse

  val GET, HEAD, POST, PUT, DELETE, TRACE, OPTIONS, CONNECT, PATCH = Value

  // Doesn't yet work
  val GET, HEAD: Value, POST, PUT, DELETE, TRACE, OPTIONS, CONNECT, PATCH: Value = Value

  def x: Something#SomethingElse

  def hasFunctionType[A, B <: A, Z](f: A => B, g: (A, B) => Z): Unit = {
    println("Something")
  }

  val f: (Int, String) => Unit = (i: Int, s: String) => println(s"$i -- $s")
  val f: (Int, String) ⇒ Unit = (i: Int, s: String) ⇒ println(s"$i -- $s")
}

object Test {
  def test(
    param1: List[(Int, Int)],
    param2: List[Int]):
  List[Int] = {
    param2 match {
      case head :: tail => tail
    }
  }
}

case class ACaseClass(param1: Float = 14.23f)
case object ACaseObject extends Something

def x(): Unit = {
  case Something(a, b) =>
  case SomethingElse() =>
  case SomethingElseElse =>
}

class ScalaClass(i: Int = 12, b: Trait[A, Trait[B, C]]) extends B with SomeTrait[A, B[String], D] {
  /**
   * I forgot comments! We spelcheck them. [[scala.Option]]
   *
   * {{{
   * scala> This is a REPL line
   * scala> and this is another one
   * }}}
   *
   * <li></li>
   *
   * @param parameter Explanation of the parameter. Speling.
   * @return TODO
   */
  val thing = "A String" // this is a trailing comment, spelchecked too [TODO]
  val thing = "A String with a \" in it"
  val intString = "A string with $stuff // and a comment in it"
  val intString = s"A string /* a comment and */ with $stuff and ${stuff} in it"
  val intString = s"""A string /* a comment and */ with $stuff and ${stuff} in it"""
  val intFString = f"A string with $stuff and ${stuff} and ${eval this}%-2.2f and $stuff%2d in it"
  val intFString = f"""A string with $stuff and ${stuff} and ${eval this}%-2.2f and $stuff%2d in it"""
  val otherThings = """|This is a string
                       |that spans multiple lines.
                       |""".stripMargin
  val intString = sql"select * from T where id = $id and name = ${name}"
  val intString = sql"""
    select * from T
    where id = $id and name = ${s"$name Jr"} and age > ${age + 10}
  """

  val notImplemented = ???

  implicit val somethingImplicit = true

  // Ripped off from Scalaz
  final def foldMap[B: Monoid](f: A => B = (a: A) => A): B = F.foldMap(self)(f)
  final def foldRight[B](z: => B)(f: (A, => B) => B): B = F.foldRight(self, z)(f)
  final def foldLeft[B](z: B)(f: (B, A) => B): B = F.foldLeft(self, z)(f)
  final def foldRightM[G[_], B](z: => B)(f: (A, => B) => G[B])(implicit M: Monad[G]): G[B] = F.foldRightM(self, z)(f)
  final def foldLeftM[G[_], B](z: B)(f: (B, A) => G[B])(implicit M: Monad[G]): G[B] = F.foldLeftM(self, z)(f)
  final def foldr[B](z: => B)(f: A => (=> B) => B): B = F.foldr(self, z)(f)
  final def foldl[B](z: B)(f: B => A => B): B = F.foldl(self, z)(f)
  final def foldrM[G[_], B](z: => B)(f: A => ( => B) => G[B])(implicit M: Monad[G]): G[B] = F.foldrM(self, z)(f)

  val aChar = 'a'
  val anEscapedChar = '\\'
  val anotherEscapedChar = '\n'
  val aUnicodeChar = '\u00ab'
  val aSymbol = 'SomeSymbol
  def number = 0xAf903adeL
  def float = 1f
  def float = 1F
  def float = 1.1f
  def float = 1.1F
  def float = 231.1232f
  def float = 231.2321F
  def float = .2f
  def float = .2F
  def double = 1d
  def double = 1D
  def double = 1.1d
  def double = 1.1D
  def double = 231.1232d
  def double = 231.2321D
  def double = 231.2321
  def double = .2d
  def double = .2
  def double = .2D
  def exp = 1.2342e-24
  def exp = 1e+24
  var flarf: Int = 12
  def flooger(x: String): Unit = println(42)
  private val booger = "Hithere"
  protected[this] def something[A](y: SomeTrait[A])(implicit shoot: Function[Int, String]): Long = 12
  private final val do = done

  someVar match {
    case Flooger(thing, that, matches) =>
      flender ! Message(hi, there, guys)
    case '"' => Bah
  }

  try {
    whatever
  } catch {
    case e: Throwable
  } finally {
    at the end
  }

  while (a == b) {
  }

  for (x <- somecall) {
    dothing
  }

  for {
    a <- futureCall1
    b <- futureCall2
  } yield (a, b)

  protected[package] something = null

  def receive = super.receive

  require(something == true)

  val q"This $is a $string" = something

  q"""return this $thing"""
  tq"""return this $thing"""
  tq"return this $thing"
  cq"""return this $thing"""
  cq"return this $thing"
  pq"""return this $thing"""
  pq"return this $thing"

  val something = s"""bar="foo""""
  val something = f"""bar="foo""""
  val something = """bar="foo""""
  val something = s"Interpolatin' fancy expressions ${bar map (_.toString)}"

  def someFunc[A <: B, X =:= Y]

  val soManyEscapes = "\\\"\u0031\n\b\r\f\t" // and a comment
  val soManyEscapes = """\\\"\u0031\n\b\r\f\t""" // and a comment
  val soManyEscapes = s"\\\"\u0031\n\b\r\f\t" // and a comment
  val soManyEscapes = f"\\\"\u0031\n\b\r\f\t" // and a comment
  val soManyEscapes = s"""\\\"\u0031\n\b\r\f\t""" // and a comment
  val soManyEscapes = f"""\\\"\u0031\n\b\r\f\t""" // and a comment
  val soManyEscapes = "\\\"\u0031\n\b\r\f\t" // and a comment
}
