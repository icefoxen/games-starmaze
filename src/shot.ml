(* shot.ml
   A gameobj that deals damage to whatever hits it and has a limited
   lifespan.

   ...make this part of the particles?
   Well.  We probably WILL want it to scatter particles all over at
   one point or another, for impact effects or such.

   Simon Heath
   4/11/2006
*)


open Util;;

let drawNothing scr obj =
  ()
;;

let drawCircle scr obj =
  let area = obj#getArea in
  let nx = x2screen area#getx !logscreenx
  and ny = y2screen area#gety !logscreeny in
    Drawing.drawFilledCircle scr (255,255,255) nx ny (int_of_float (area#getw))
;;


class shot cfgfile player =
  let cfg = Resources.getConfig cfgfile in
object (self)
  inherit Gameobj.gameobj cfgfile

  val damageDone = cfg#getFloat "shot" "damage"
  val mutable timeLeft = cfg#getInt "shot" "lifetime"
  val mutable facing = left
(*  val drawFuncName = cfg#getStr "shot" "drawFunc" *)
  val mutable drawFunc = drawNothing  (* We use a similar technique for particles *)

  val element = Gameobj.str2element (cfg#getStr "shot" "element")

  initializer
    self#setDrawFunc (cfg#getStr "shot" "drawFunc");
    vector#setxy (cfg#getFloat "shot" "xspeed") (cfg#getFloat "shot" "yspeed");
    self#setFacing (player#getFacing);
    vector#setx ((vector#getx *. facing) +. (player#getVector#getx));
    let x, y = player#getReticle in
      self#moveTo x y;


  method setFacing dir =
    facing <- dir

  method drawCircle scr =
    let nx = x2screen area#getx !logscreenx
    and ny = y2screen area#gety !logscreeny in
      Drawing.drawFilledCircle scr (255,255,255) nx ny (int_of_float (area#getw))


  method calculate (t : int) =
    self#doGravity t;
    area#moveto (vector#movex area#getx t) (vector#movey area#gety t);

    timeLeft <- timeLeft - t;
    if timeLeft < 0 then
      self#die;

  method collideWithObject (obj : Gameobj.gameobj) =
    if area#isColliding obj#getArea then (
	obj#damage element damageDone;
	self#die;
      )

  method draw scr =
    drawFunc scr self;

  method setDrawFunc d = 
    match d with
	"circle"     -> drawFunc <- drawCircle
      | "fireClose"  -> ()
      | "fireFar"    -> ()
      | "earthClose" -> ()
      | "earthFar"   -> ()
      | "waterClose" -> ()
      | "waterFar"   -> ()
      | "airClose"   -> ()
      | "airFar"     -> ()
      | _            -> drawFunc <- drawNothing
	  



end;;
