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



type element =
    Fire
  | Water
  | Earth
  | Air
  | Start
  | End
  | Soul
  | Star
;;

let str2element = function
    "fire" -> Fire
  | "water" -> Water
  | "earth" -> Earth
  | "air" -> Air
  | "start" -> Start
  | "end" -> End
  | "soul" -> Soul
  | "star" -> Star
  | s -> Util.error "You horrible, horrible person!  %s\n" s
;;

class gameobj cfgfile =
  let cfg = getConfig cfgfile in
object (self)
  val mutable area = new area cfgfile
  val mutable vector = new vector

  val mutable hits = cfg#getFloat "gameobj" "hits"
  val mutable maxhits = cfg#getFloat "gameobj" "hits"
    
  val mutable alive = true
  val mutable visible = true
  val mutable hasWeight = cfg#getBool "gameobj" "hasWeight";

  val mutable onGround = true

  (* Elemental resist values.  They're basically damage multipliers. *)
  val mutable fireResist = cfg#getFloat "gameobj" "fireResist"
  val mutable waterResist = cfg#getFloat "gameobj" "waterResist"
  val mutable earthResist = cfg#getFloat "gameobj" "earthResist"
  val mutable airResist = cfg#getFloat "gameobj" "airResist"
  val mutable startResist = cfg#getFloat "gameobj" "startResist"
  val mutable endResist = cfg#getFloat "gameobj" "endResist"
  val mutable soulResist = cfg#getFloat "gameobj" "soulResist"
  val mutable starResist = cfg#getFloat "gameobj" "starResist"

  method acceldm d m =
    vector#accel_by_dirmag d m

  method accelv v =
    vector#add v

  method accel xoff yoff =
    vector#addxy xoff yoff

  method calculate t =
    self#doGravity t;
    self#doFriction 0.98;
    area#moveto (vector#movex area#getx t) (vector#movey area#gety t);
    

  (* We only do gravity if we're not on the ground!  Whoo. 
     This works because we calculate, then set whether we're on the ground
     or not as part of the collision detection.
  *)
  method doGravity t =
    if hasWeight && (not onGround) then (
	vector#addxy 0. (!Util.gravity *. (float_of_int t));
      )

  method doFriction friction =
    if onGround then
      vector#setx (vector#getx *. friction);
    (* Make life nicer by making asymptotes not go to infinity. *)
    if (vector#getx < 0.1) && (vector#getx > -0.1) then 
      vector#setx 0.0

  (* Experimental, not used! *)
  (*
    method doLineTerrainCollision line =
    let x = area#getx
    and y = area#gety
    and w = area#getw
    and h = area#geth
    and vx = vector#getx
    and vy = vector#gety in
  (* top-left, top-right, bottom-left, bottom-right *)
    let tl = (x, y, x+.vx, y+.vy)
    and tr = (x+.w, y, x+.w+.vx, y+.vy)
    and bl = (x, y-.h, x+.vx, y-.h+.vy)
    and br = (x+.w, y-.h, x+.w+.vx, y-.h+.vy) in
    let b1, x1, y1 =(linesColliding line tl)
    and b2, x2, y2 =(linesColliding line tr)
    and b3, x3, y3 =(linesColliding line bl)
    and b4, x4, y4 =(linesColliding line br) in

    if  (vy >= 0.) && (vx <= 0.) &&  b1 then (
    print_endline "Case 1";
    area#moveto x1 y1;
    vector#sety 0.;
    ) else if  (vy >= 0.) && (vx >= 0.) &&  b2 then (
    print_endline "Case 2";
    area#moveto x2 y2;
    vector#sety 0.;
    ) else if (vy <= 0.) && (vx <= 0.) && b3 then (
    print_endline "Case 3";
    area#moveto x3 (y3 +. area#geth +. 1.);
    vector#sety 0.;
    self#setOnGround true;
    ) else if (vy <= 0.) && (vx >= 0.) && b4 then (
    print_endline "Case 4";
    area#moveto (x4 -. w) (y4 +. area#geth +. 1.);
    vector#sety 0.;
    self#setOnGround true;
    )
  *)




  (* Terrain is represented by a set of lines...
     W00t!  It works!
     For bounding boxes and lines, at least.  For lines and lines...

     Hokay... for lines on lines, we turn the vector into a line and
     move it to the location of the object, then try to collide it.
     This could be more efficient by cacheing the vectorline each
     tick, in #calculate.

     So we do both bounding-box collision, which works well when things
     are large, and line collision, which works well when things are small.
     The latter isn't perfect still, for some reason... shots still go
     through floors.
     
  *)
  method doTerrainCollision line (t : int) =
    (*    if area#istangible then *)

    if collidingWithLine area line then (
	if lineIsHorz line then (
	    let _, y, _, _ = line in 
	      (* Technically incorrect, but it ensures we're on
		 the ground. *)
	      if vector#gety = 0.0 then
		self#setOnGround true
	      else if vector#gety < 0.0 then (
		  area#sety (y +. area#geth);
		  self#setOnGround true;
		)
	      else (
		  area#sety y;
		);
	      vector#sety 0.0;

	  )
	else if lineIsVert line then ( 
	    let x, _, _, _ = line in
	      if vector#getx = 0.0 then
		()
	      else if vector#getx < 0.0 then
		area#setx x
	      else
		area#setx (x -. area#getw);
	      vector#setx 0.0;
	  )

      (* dt is the velocity for time correction, from vector.ml 
	 A bit disorganized, buuuut...
      *)
      (*
	) else (
	let dt = (float_of_int t) *. 0.01 in
	let vx = vector#getx *. dt
	and vy = vector#gety *. dt
	and ax = area#getx
	and ay = area#gety 
	and aw = area#getw
	and ah = area#geth in
	let lx, ly, lw, lh = line in
	if lineIsVert line then ( 
	if ay > ly && (ay +. ah) < (ly +. lh) then
	if (ax +. aw) < lx && (ax +. aw +. vx) > lx then (
	area#setx (lx -. aw);
	vector#setx 0.0;
	)
	else if ax > lx && (ax +. vx) < lx then (
	area#setx lx;
	vector#setx 0.0;
	)
	) 

	else if lineIsHorz line then ( 
	if ax > lx && (ax +. aw) < (lx +. lh) then
	if (ay -. ah) > ly && (ay -. ah +. vy) < ly then (
	area#sety (ly +. ah +. 1.);
	vector#sety 0.0;
	self#setOnGround true;
	)
	else if ay < ly && (ay +. vy) > ly then (
	area#sety ly;
	vector#sety 0.0;
	)

	);
      *)
      ); 

    
  method collideWithObject (obj : gameobj)= 
    if area#isColliding obj#getArea then (
	self#handleCollision obj;
      )

  method handleCollision (obj : gameobj) =
    let ar = obj#getArea in
    let myCenterX = area#getx +. (area#getw /. 2.)
    and myCenterY = area#gety -. (area#geth /. 2.)
    and otherCenterX = ar#getx +. (ar#getw /. 2.)
    and otherCenterY = ar#gety -. (ar#geth /. 2.) in

    let dx = otherCenterX -. myCenterX
    and dy = otherCenterY -. myCenterY in
    let xadjust = if dx > 0. then 10. else if dx < 0. then -10. else 0.
    and yadjust = if dy > 0. then 15. else if dy < 0. then -15. else 0.
    in
	
	self#getVector#setxy 0. 0.;
      self#accel (-.xadjust) (-.yadjust);
      obj#getVector#setxy 0. 0.;
      obj#accel (xadjust) (yadjust);


  (* Presumably this'll do an explosion animation or something, plus playing
     a sound, setting score/relations, removing self from the level, etc.

     Well, make particles at least.  But any actual effect on the world
     around it gets more complicated...
  *)
  method die =
    alive <- false;


  (* If damage starts out negative, it's invulnerable. 
     Negative damage doesn't happen.
  *)

  method getElementResist element =
    match element with
	Fire -> fireResist
      | Water -> waterResist
      | Earth -> earthResist
      | Air -> airResist
      | Start -> startResist
      | End -> endResist
      | Soul -> soulResist
      | Star -> starResist


  method setElementalResist element x =
    match element with
	Fire -> fireResist <- x
      | Water -> waterResist <- x
      | Earth -> earthResist <- x
      | Air -> airResist <- x
      | Start -> startResist <- x
      | End -> endResist <- x
      | Soul -> soulResist <- x
      | Star -> starResist <- x

  method damage element x =
    if hits < 0. then
      ()
    else (
	hits <- hits -. ((absf x) *. self#getElementResist element);
	if hits < 0. then (
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

  method setXY x' y' =
    area#setxy x' y'

  method addX x' =
    area#setx (area#getx +. x')

  method addY y' =
    area#sety (area#gety +. y')

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
  method draw scr =
    let centerx = area#getx +. (area#getw /. 2.)
    and centery = area#gety -. (area#geth /. 2.) in
    let radius = (int_of_float (area#getw /. 2.)) in
    let nx = x2screen centerx !logscreenx
    and ny = y2screen centery !logscreeny in
      Drawing.drawCircle scr (255,255,255) nx ny radius;
      Drawing.drawCircle scr (255,255,255) nx ny (radius / 2);


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
      Drawing.drawLine dst Sdlvideo.green x1 x2 y1 y2;

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
  method setHits x = hits <- max x maxhits

  method getVector = vector
  method setVector v =
    vector <- v


  method print =
    area#print;
    vector#print;


end;;


