

open Line

class poly l =
object (self)
  val mutable lines = l

  val mutable memoizedW = 0.
  val mutable memoizedH = 0.

  initializer
    self#checkLines


  (* Update the memoized attributes *)
  method private checkLines =
    let maxx = ref 0.
    and minx = ref 0.
    and maxy = ref 0.
    and miny = ref 0. in
    let checkLine l = 
      let x0, y0, x1, y1 = l in
	maxx := (max !maxx (max x0 x1));
	maxy := (max !maxy (max y0 y1));
	minx := (min !minx (min x0 x1));
	miny := (min !miny (min y0 y1));
    in
      List.iter checkLine lines;
      memoizedW <- !maxx -. !minx;
      memoizedH <- !maxy -. !miny;
      

  method getw =
    memoizedW
      
  method geth = 
    memoizedH

  method addLine l =
    lines <- l :: lines;
    self#checkLines

  method addLines l = 
    lines <- lines @ l;
    self#checkLines

  method lineIntersects line =
    (* Recursion > iteration, at least in ocaml. *)
    let rec loop lines = 
      if lines = [] then
	(false, 0., 0.)
      else
	let first = List.hd lines
	and rest = List.tl lines in
	let b, x, y = linesColliding line first in
	  if b then
	    (true, x, y)
	  else
	    loop rest
    in
      loop lines

end;;
