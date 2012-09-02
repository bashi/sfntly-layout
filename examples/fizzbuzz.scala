import net.maniacchef.sfntly.layout.Gsub
Gsub("ubuntu-mono.ttf").contextualAlternate {
  List.range(1, 101).map { i => ((i % 3), (i % 5)) match {
    case (0, 0) => (i.toString -> "FizzBuzz")
    case (0, _) => (i.toString -> "Fizz")
    case (_, 0) => (i.toString -> "Buzz")
    case _ => Unit
  }}
}.done("ubuntu-mono-fizzbuzz.ttf")
