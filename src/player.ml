(* player.ml
   A player object.
   Yay!

   Simon Heath
   12/3/2005
*)

open Util;;
open Particles;;




class player cfgfile =
  let cfg = Resources.getConfig cfgfile in
object (self)
  inherit Gameobj.gameobj cfgfile
  val mutable accel = cfg#getFloat "player" "accel"
  val mutable maxspeed = cfg#getFloat "player" "maxspeed"

(*
  val mutable boostmaxspeed = cfg#getFloat "player" "boostmaxspeed"
  val mutable boostaccel = cfg#getFloat "player" "boostaccel"
  val mutable boostlength = cfg#getInt "player" "boostlength" (* in ms *)
*)


  (* Location and rect and so on is done by the gameobj superclass *)
  val mutable facing = right
(*  val mutable boosting = false *)

  val mutable energy = 100.0
  val mutable maxEnergy = 100.0

  val mutable energyRegenPerMS = 0.01



  method calculate t =
    self#doGravity t;
    self#doFriction 0.98;
    self#doEnergyRegen t;
    area#moveto (vector#movex area#getx t) (vector#movey area#gety t);

  method doEnergyRegen t =
    if energy < maxEnergy then (
	energy <- min 
	  (energy +. (energyRegenPerMS *. (float_of_int t)))
	  maxEnergy
      )

  method getEnergy =
    energy

  method setEnergy e =
    energy <- e

  method removeEnergy e =
    if energy >= e then
      energy <- energy -. e


  method doWithEnergyCost thunk e =
    if energy >= e then (
      energy <- energy -. e;
      thunk ()
    )
    else
      ()

  method setMaxEnergy e =
    maxEnergy <- e;
    energy <- min energy maxEnergy;

  method onGround =
    onGround

  method setOnGround x =
    onGround <- x

  method doBrake =
    if onGround then (
      self#doFriction 0.90;
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


  method draw scr =
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

	let randx = (Random.float 20.) -. 10.
	and randy = (Random.float 20.) -. 10. in



	  self#doFlameParticle 
	    (centerx -. xoff2 +. randy) (centery +. yoff2 +. randx);
	  self#doWaterParticle 
	    (centerx +. xoff2 -. randy) (centery -. yoff2 -. randx);

	  self#doAirParticle  
	    (centerx +. xoff -. randx) (centery -. yoff -. randy);
	  self#doEarthParticle
	    (centerx -. xoff +. randx) (centery +. yoff +. randy);


	  self#drawReticle scr (255,255,255);

	  
  method doFlameParticle x y =
    particles#make "FlameParticle.cfg" x y

  method doAirParticle x y =
    particles#make "AirParticle.cfg" x y

  method doEarthParticle x y =
    particles#make "EarthParticle.cfg" x y

  method doWaterParticle x y =
    particles#make "WaterParticle.cfg" x y

  method drawReticle scr color =
    let x, y = self#getReticle in
    let scrx = x2screen x !Util.logscreenx
    and scry = y2screen y !Util.logscreeny in
      Drawing.drawCircle scr color scrx scry 2;

  method getReticle =
    let centerx = area#getx +. (area#getw /. 2.)
    and centery = area#gety -. (area#geth /. 2.) in
      ((centerx +. (30. *. facing)), centery)

  method getFacing =
    facing
	




	

end;;
