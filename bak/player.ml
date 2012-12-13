(* player.ml
   A player object.
   Yay!

   Simon Heath
   12/3/2005
*)

open Util;;
open Particles;;


let right = 1.
and left = -1.;;



class player cfgfile =
  let cfg = Resources.getConfig cfgfile in
object (self)
  inherit Gameobj.gameobj cfgfile
  val mutable score = 0;
  val mutable lives = 0;
  val mutable crystals = 0;
  val mutable shield = false;
  val mutable invulnerable = false
  val mutable accel = cfg#getFloat "player" "accel"
  val mutable maxspeed = cfg#getFloat "player" "maxspeed"
  val mutable boostmaxspeed = cfg#getFloat "player" "boostmaxspeed"
  val mutable boostaccel = cfg#getFloat "player" "boostaccel"
  val mutable boostlength = cfg#getInt "player" "boostlength" (* in ms *)
  val mutable jumppower = cfg#getFloat "player" "jumppower"

  (* Location and rect and so on is done by the gameobj superclass *)
  val mutable facing = right
  val mutable boosting = false



  method calculate t =
    area#moveto (vector#movex area#getx t) (vector#movey area#gety t);

  method doBoost = 
    let x = vector#getx in
      if (absf x) >= boostmaxspeed then (

	();
      )
      else (
	vector#addxy (boostaccel *. facing) 0.;
      )

  method doJump = 
    if onGround then (
      onGround <- false;
      vector#addxy 0. jumppower;
    )

  method doBrake =
    if onGround then (
      self#doFriction 0.95;
    )


  (* This needs some funky heuteristics.
     We should be able to go over our max speed, and walking in the
     same direction should not immediately bring us down to max speed.
     But we still want to be able to walk in the opposite direction to
     slow down.

     So if movement and facing are in the same direction, we do the acceleration.
     Otherwise, we don't.

  *)
  method doWalk direction = 
    let x = vector#getx in
      if ((x *. direction) > 0.) && ((absf x) >= maxspeed) then (
	();
      )
      else (
	facing <- direction;
	vector#addxy (accel *. facing) 0.;
      )


  method draw scr (timePassed : int) =
    let centerx = area#getx +. (area#getw /. 2.)
    and centery = area#gety -. (area#geth /. 2.) in
    let radius = (int_of_float (area#getw /. 2.)) in
    let nx = x2screen centerx !logscreenx
    and ny = y2screen centery !logscreeny in
      Drawing.drawCircle scr (255,255,255) nx ny radius;
      Drawing.drawCircle scr (255,255,255) nx ny (radius - 3);

      (* There has REALLY REALLY got to be a simpler way of doing
	 this, but this works for now. *)
      let xoff = (cos (centerx /. 30.)) *. 30.
      and yoff = (sin (centerx /. 30.)) *. 30. in

      let xoff2 = (cos ((centerx /. 30.) +. (pi/.2.))) *. 30.
      and yoff2 = (sin ((centerx /. 30.) +. (pi/.2.))) *. 30. in

      let nxoff1 = x2screen (centerx -. xoff) !logscreenx
      and nyoff1 = y2screen (centery +. yoff) !logscreeny in
      let nxoff2 = x2screen (centerx +. xoff) !logscreenx
      and nyoff2 = y2screen (centery -. yoff) !logscreeny in
      let nxoff3 = x2screen (centerx -. xoff2) !logscreenx
      and nyoff3 = y2screen (centery +. yoff2) !logscreeny in
      let nxoff4 = x2screen (centerx +. xoff2) !logscreenx
      and nyoff4 = y2screen (centery -. yoff2) !logscreeny in

	Drawing.drawLine scr (128,128,128) nx nxoff1 ny nyoff1;
	Drawing.drawLine scr (128,128,128) nx nxoff2 ny nyoff2;
	Drawing.drawLine scr (128,128,128) nx nxoff3 ny nyoff3;
	Drawing.drawLine scr (128,128,128) nx nxoff4 ny nyoff4;


	particles#make "BlueParticle.cfg" (centerx -. xoff) (centery +. yoff);
	particles#make "GreenParticle.cfg" (centerx +. xoff) (centery -. yoff);
	particles#make "RedParticle.cfg" (centerx -. xoff2) (centery +. yoff2);
	particles#make "YellowParticle.cfg" (centerx +. xoff2) (centery -. yoff2);

(*
	let p1 = makeBlueParticle (centerx -. xoff) (centery +. yoff) in
	let p2 = makeGreenParticle (centerx +. xoff) (centery -. yoff) in
	let p3 = makeRedParticle (centerx -. xoff2) (centery +. yoff2) in
	let p4 = makeWhiteParticle (centerx +. xoff2) (centery -. yoff2) in
	  particles#add p1;
	  particles#add p2;
	  particles#add p3;
	  particles#add p4;
*)




	  

end;;
