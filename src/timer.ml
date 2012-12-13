(* timer.ml
   Simple timers for doing things like, say, timing enemy behavior or
   seeing how long a pillar of fire should appear.

   Simon Heath
*)



(* How should these work, anyway?
   Do function after X, up to X times?
  
   Most of what I use timers for is "Has X time passed since Y?"
   and "How much time has passed since Y?", which I suppose are
   really the same thing...
*)
class timer =
object (self)

  val mutable currentTime = 0
  val mutable interval = 0
  val mutable lastInterval = 0

  method setInterval x =
    interval <- x

  method update t =
    currentTime <- t


  method intervalIsPassed =
    0

  method reset =
    ()

end;;
