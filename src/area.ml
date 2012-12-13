(* area.ml
   A class for an "area", which is anything that takes up space.
   x is the left side, y is the top edge.

   Simon Heath
*)

open Util;;
open Config;;
open Resources;;

let notColliding = 0
and top = 1
and right = 2
and bottom = 3
and left = 4;;


class area cfgfile =
  let cfg = Resources.getConfig cfgfile  in
object (self)

  val mutable x = cfg#getFloat "area" "x"
  val mutable y = cfg#getFloat "area" "y"
  val mutable w = cfg#getFloat "area" "w"
  val mutable h = cfg#getFloat "area" "h"
  val mutable tangible = cfg#getBool "area" "tangible"


  method getx = x
  method gety = y
  method getw = w
  method geth = h
  method istangible = tangible

  method addx n = x <- x +. n
  method addy n = y <- y +. n

  method setx n = x <- n
  method sety n = y <- n
  method setxy nx ny =
    x <- nx;
    y <- ny;

  method setw n = w <- n
  method seth n = h <- n
  method setTangible n = tangible <- n
  method moveto nx ny =
    x <- nx;
    y <- ny

  method setsize nw nh =
    w <- nw;
    h <- nh

  (* Old collision detection algorithm using bounding-boxes.
     Not quite right...
     The smegging bounding-boxes are offset.  Curses.
  *)

  method isColliding (ar : area) =
    if tangible && ar#istangible then
      (*      let left1 = x  +. (w *. 0.1)
	      and left2 = ar#getx +. (ar#getw *. 0.1) 
	      and bottom1 = y +. (h *. 0.1)
	      and bottom2 = ar#gety +. (ar#geth *. 0.1)
	      and right1 = x +. (w *. 0.9)
	      and right2 = ar#getx +. (ar#getw *. 0.9)
	      and top1 = y +. (h *. 0.9) 
	      and top2 = ar#gety +. (ar#geth *. 0.9) in
      *)
      let left1 = x
      and left2 = ar#getx
      and bottom1 = y -. h
      and bottom2 = ar#gety -. ar#geth
      and right1 = x +. w
      and right2 = ar#getx +. ar#getw
      and top1 = y
      and top2 = ar#gety in
	if bottom1 > top2 then (
	    (*	    print_endline "I'm above the target";
		    Printf.printf "I'm %f, he's %f\n" bottom1 top2;
	    *)
	    false
	  )
	else if top1 < bottom2 then (
	    (*	    print_endline "I'm below the target"; *)
	    false
	  )
	else if right1 < left2 then (
	    (*	    print_endline "I'm to the left of the target"; *)
	    false
	  )
	else if left1 > right2 then (
	    (*	    print_endline "I'm to the right of the target"; *)
	    false
	  )
	else (
	    (*	    print_endline "Collision made!"; *)
	    true
	  )
	else (
	    (* print_endline "Collision impossible!"; *)
	    false
	  )

  method toLines =
    let l1 = (x, y, x+.w, y)
    and l2 = (x, y, x, y-.h)
    and l3 = (x+.w, y, x+.w, y-.h)
    and l4 = (x, y-.h, x+.w, y-.h) in
      [l1;l2;l3;l4]


  (* Randomizes an area within certain constraints *)
  method randomize conx cony =
    let rndx = Random.float conx
    and rndy = Random.float cony
    in
      x <- x +. rndx -. (rndx /. 2.);
      y <- y +. rndy -. (rndy /. 2.)

  method print =
    Printf.printf "Area: x: %f y: %f w: %f h: %f: tangible: %b\n"
      x y w h tangible


  method draw scr =
    let dx = x2screen x !logscreenx
    and dy = y2screen y !logscreeny in
      Drawing.drawRect scr (0,255,0) dx dy (int_of_float w) (int_of_float h);
      Drawing.drawRect scr (255,0,0) dx dy 2 2;



end;;
