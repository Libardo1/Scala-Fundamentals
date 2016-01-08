package example

/**
  * Created by marvinbertin on 1/6/16.
  */
object Main extends App {
  println(Lists.sum(List(1,3,2)))
  println(Lists.max(List(1,3,2)))

  println(Lists.sum(List()))
  println(Lists.max(List()))

}
