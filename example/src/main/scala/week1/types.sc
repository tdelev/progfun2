
package object test {

  println("Hello")

  val f: String => String = {
    case "ping" => "pong"
  }

  f("ping")
  println("out")
}
