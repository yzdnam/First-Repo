fun f x = x + x
fun h () = (print "hi"; f)
val g = (h ())
