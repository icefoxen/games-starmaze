(* gameobj.ml
   Game object.  It moves, it accelerates and decellerates, it bounces off
   other things and doesn't go through them, it is affected by gravity.

   I think I shall replace sprites with just an arbitrary function that takes
   self, time, screen, and particle engine.
   Instead of the function being a variable, maybe we can make a different
   type of gameobj for each kind of thing?  Sorta makes sense, but I dislike
   zillions of subclasses.
   Well, KISS for now.

   What kind of gameobjs will I need?
   FX particles of various types, weapon particles of various types, 
   player, enemy, power-ups, 

   Simon Heath
   11/3/2005
*)

open Resources;;
open Area;;
open Vector;;
open Util;;
open Line;;


class gameobj cfgfile =
  let cfg = getConfig cfgfile in
object (self)
  val mutable area = new area cfgfile
  val mutable vector = new vector

  val mutable hits = cfg#getInt "gameobj" "hits"
    
  val mutable alive = true
  val mutable visible = true
  val mutable hasWeight = cfg#getBool "gameobj" "hasWeight";

  val mutable onGround = true

  method acceldm d m =
    vector#accel_by_dirmag d m

  method accelv v =
    vector#add v

  method accel xoff yoff =
    vector#addxy xoff yoff

  method calculate t =
    area#moveto (vector#movex area#getx t) (vector#movey area#gety t);
    

  method doGravity t =
    if hasWeight then
      vector#addxy 0. (!Util.gravity *. (float_of_int t))
	(*    if vector#gety < !gravlimit then
	      vector#sety !gravlimit;
	*)

  method doFriction friction =
    if onGround then
      vector#setx (vector#getx *. friction);
    (* Make life simpler by catching point of diminishing returns. *)
    if (vector#getx < 0.1) && (vector#getx > -0.1) then 
      vector#setx 0.0


  (* Terrain is represented by a set of lines...
     W00t!  It works!
     For bounding boxes and lines, at least.  For lines and lines...

     Hokay... for lines on lines, we turn the vector into a line and
     move it to the location of the object, then try to collide it.
     This could be more efficient by cacheing the vectorline each
     tick, in #calculate.
     
  *)
  method doTerrainCollision line =
    if area#istangible then
      (*
	let vx = area#getx +. vector#getx
	and vy = area#gety +. vector#gety in
	let vectorline = Line.ALine( area#getx, area#gety, vx, vy ) in

	let toprightline = 
	Line.moveLine vectorline (area#getx /. 2.) (area#gety /. 2.)
	and topleftline = 
	Line.moveLine vectorline (-.area#getx /. 2.) (area#gety /. 2.)
	and bottomrightline =
	Line.moveLine vectorline (area#getx /. 2.) (-.area#gety /. 2.)
	and bottomleftline  =
	Line.moveLine vectorline (-.area#getx /. 2.) (-.area#gety /. 2.) in

	let b, x, y = Line.linesColliding line vectorline in
	if b then (
      (*	    if vector#getx < 0.0 then
	area#setx x
	else
	area#setx (x -. area#getw); *)
	if vector#gety < 0.0 then (
	area#sety (y +. area#geth);
	self#setOnGround true;
	)
	else
	area#sety y;

      (*	    area#setxy (x -. vector#getx -. area#getw) 
	(y -. vector#gety +. area#geth); *)
	vector#setxy 0.0 0.0;
	)
      *)


      match line with
	  VLine( x, y, height ) ->
	    if collidingWithVLine area x y height then (
	      if vector#getx < 0.0 then (
		area#setx x;
	      )
	      else (
		area#setx (x -. area#getw);
	      );
	      vector#setx 0.0;
	    )
	| ALine( _ ) ->
	    print_endline "Foo!";
	| HLine( x, y, length ) ->
	    if collidingWithHLine area x y length then (
	      if vector#gety < 0.0 then (
		area#sety (y +. area#geth);
		self#setOnGround true;
	      )
	      else (
		area#sety (y)
	      );
	      (* Implement a bit of bounce.  Neat, but it also gives
		 you a bit of a boost on your next jump; to limit that
		 we'd have to make a max vertical velocity, which I don't
		 want to bother with.
	      *)
	      (*	      vector#sety (-.(vector#gety /. 3.)); *)
	      vector#sety 0.;
	    )



  (* Presumably this'll do an explosion animation or something, plus playing
     a sound, setting score/relations, removing self from the level, etc.
  *)
  method die =
    alive <- false;


  (* If damage starts out negative, it's invulnerable. *)
  method damage x =
    if hits < 0 then
      ()
    else (
      hits <- hits - x;
      if hits < 0 then (
	alive <- false;
	self#die
      )
    )


  method setSize w' h' =
    area#setsize w' h'

  method getH = area#geth
  method setH n = 
    area#seth n

  method getW = area#getw
  method setW n = 
    area#setw n

  method getX = area#getx
  method getY = area#gety

  method setX x' =
    area#setx x'

  method setY y' =
    area#sety y'

  method moveTo x' y' =
    area#moveto x' y'

  method isOnGround =
    onGround

  method setOnGround onground =
    onGround <- onground


  (* ...OCaml is surprisingly resistant to "self" being passed to, like,
     any member variable.
     We'll just subclass, dammit.
  *)
  method draw scr (timePassed : int) =
    let centerx = area#getx +. (area#getw /. 2.)
    and centery = area#gety -. (area#geth /. 2.) in
    let radius = (int_of_float (area#getw /. 2.)) in
    let nx = x2screen centerx !logscreenx
    and ny = y2screen centery !logscreeny in
      Drawing.drawCircle scr (255,255,255) nx ny radius


  (* This draws a rectangle around the sprite's bounding box.
     Well, the sprite's area's edges, really.  They're not exactly the same,
     since area#isColliding takes each edge in by 10%.  Close enough.
  *)
  method drawBox dst scrx scry =
    let nx = (x2screen area#getx scrx)
    and ny = (y2screen area#gety scry)
    and w = (int_of_float area#getw)
    and h = (int_of_float area#geth) in
      Drawing.drawRect dst (Sdlvideo.green) nx ny w h;


  (* This draws the object's vector as a line. *)
  method drawVector dst scrx scry =
    let x1 = (x2screen area#getx scrx)
    and y1 = (y2screen area#gety scry) in
    let x2 = x1 + (int_of_float vector#getx)
    and y2 = y1 - (int_of_float vector#gety) in
      Drawing.drawLine dst Sdlvideo.green x1 y1 x2 y2;

  method isAlive = alive
  method isVisible = visible

  method setAlive n =
    alive <- n

  method setVisible n =
    visible <- n

  method getArea = area


  method setArea a =
    area <- a


  method getHits = hits
  method setHits x = hits <- x

  method getVector = vector
  method setVector v =
    vector <- v


  method print =
    area#print;
    vector#print;
    Printf.printf "Gameobj: hits: %d visible: %b\n" hits visible

end;;


