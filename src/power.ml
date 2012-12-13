(* power.ml
   A thing that represents an ability-set.

   Simon
*)



class power cfgfile =
  let cfg = Resources.getConfig cfgfile in
object (self)

  val name = cfg#getStr "power" "name"

  (* These things aren't infinately tweakable, but DO give us some
     basic balance paramters to mess with.

     Okay.  ALL of the energy requirements are gonna have to become
     floats.  Really now, it's just best.
  *)

  val closeEnergy = cfg#getFloat "power" "closeEnergy"
  val closeCool = cfg#getInt "power" "closeCooldown"

  val farEnergy = cfg#getFloat "power" "farEnergy"
  val farCool = cfg#getInt "power" "farCooldown"

  val jumpPower = cfg#getFloat "power" "jumpPower"
  val jumpEnergy = cfg#getFloat "power" "jumpEnergy"
  val jumpCool = cfg#getInt "power" "jumpCooldown"

  val defendEnergy = cfg#getFloat "power" "defendEnergy"
  val defendCool = cfg#getInt "power" "defendCooldown"


  val mutable cooldownTimer = 0

(*
  val mutable jumpCoolTimer = 0
  val mutable defendCoolTimer = 0
  val mutable closeCoolTimer = 0
  val mutable farCoolTimer = 0
*)

(*
  val jumpDuration = cfg#getInt "power" "jumpDuration"
  val defendDuration = cfg#getInt "power" "defendDuration"
  val closeDuration = cfg#getInt "power" "closeDuration"
  val farDuration = cfg#getInt "power" "farDuration"
*)


  method getName =
    name


  (* Each of these returns a list of new objects... 
     The powers themselves may be called by the driver rather than
     the player object...  Yeah.

     When you hit a button, it happens, keeps happening for its duration,
     then stops and can't happen again for its cooldown.  We could also
     add a chargeup parameter, but probably wouldn't use it.
  *)
  method doJump (player : Player.player) : Gameobj.gameobj list =
    []

  method stopJump (player : Player.player) : Gameobj.gameobj list =
    []

  method doDefend (player : Player.player) : Gameobj.gameobj list =
    []

  method stopDefend (player : Player.player) : Gameobj.gameobj list =
    []

  method doCloseAttack (player : Player.player) : Gameobj.gameobj list =
    []

  method stopCloseAttack (player : Player.player) : Gameobj.gameobj list =
    []

  method doFarAttack (player : Player.player) : Gameobj.gameobj list =
    []

  method stopFarAttack (player : Player.player) : Gameobj.gameobj list =
    []

  method calc dt =
(*    jumpCoolTimer <- max 0 (jumpCoolTimer - dt);
    defendCoolTimer <- max 0 (defendCoolTimer - dt);
    closeCoolTimer <- max 0 (closeCoolTimer - dt);
    farCoolTimer <- max 0 (farCoolTimer - dt);
*)
    cooldownTimer <- max 0 (cooldownTimer - dt)

  method resetTimers =
    cooldownTimer <- 0
(*
    jumpCoolTimer <- 0;
    defendCoolTimer <- 0;
    closeCoolTimer <- 0;
    farCoolTimer <- 0;
*)


end;;




class startPower cfgfile =
object (self)
  inherit power cfgfile


  (* Ignores cooldown! *)
  method doJump (player : Player.player) : Gameobj.gameobj list =
    let jump () = 
      player#setOnGround false;
      player#getVector#addxy 0. jumpPower;
    in
      if player#isOnGround then
	player#doWithEnergyCost jump jumpEnergy;
      []



  (* Limit how fast they trail?  Make them go in the same direction as the
     player?
  *)
  method doDefend (player : Player.player) : Gameobj.gameobj list =
    if (player#getEnergy >= defendEnergy) && cooldownTimer <= 0 then (
	let locx = (player#getX +. (player#getW /. 2.)) 
	and locy = (player#getY -. (player#getH /. 2.)) in
	  Particles.particles#make "ShieldParticle.cfg" locx locy;
	  player#removeEnergy defendEnergy;
	  cooldownTimer <- defendCool
      )
    else
      ();
    []




  method doCloseAttack (player : Player.player) : Gameobj.gameobj list =
    if (player#getEnergy >= closeEnergy) && cooldownTimer <= 0 then (
	player#removeEnergy closeEnergy;
	cooldownTimer <- closeCool;
	let bullet1 = new Shot.shot "StartCloseShot.cfg" player 
	and bullet2 = new Shot.shot "StartCloseShot.cfg" player 
	and bullet3 = new Shot.shot "StartCloseShot.cfg" player 
	and bullet4 = new Shot.shot "StartCloseShot.cfg" player 
	and bullet5 = new Shot.shot "StartCloseShot.cfg" player in
	let bulletSpeed = Util.absf (bullet1#getVector#getx) in
	  bullet1#getVector#sety (bulletSpeed *. 0.5);
	  bullet2#getVector#sety (bulletSpeed *. 0.25);
	  bullet4#getVector#sety (bulletSpeed *. -.0.25);
	  bullet5#getVector#sety (bulletSpeed *. -.0.5);

	  bullet1#addY 6.0;
	  bullet2#addY 3.0;
	  bullet4#addY (-3.0);
	  bullet5#addY (-6.0);
	  [(bullet1 :> Gameobj.gameobj); (bullet2 :> Gameobj.gameobj);
	   (bullet3 :> Gameobj.gameobj); (bullet4 :> Gameobj.gameobj);
	   (bullet5 :> Gameobj.gameobj); ];
      )
    else
      []


  method doFarAttack (player : Player.player) : Gameobj.gameobj list =
        if (player#getEnergy >= farEnergy) && cooldownTimer <= 0 then (
	player#removeEnergy farEnergy;
	cooldownTimer <- farCool;
	let bullet = new Shot.shot "StartFarShot.cfg" player in
	  [(bullet :> Gameobj.gameobj)];
      )
    else
      []

end;;

class endPower cfgfile =
object (self)
  inherit power cfgfile

  method doJump (player : Player.player) : Gameobj.gameobj list =
    []

  method stopJump (player : Player.player) : Gameobj.gameobj list =
    []

  method doDefend (player : Player.player) : Gameobj.gameobj list =
    []

  method stopDefend (player : Player.player) : Gameobj.gameobj list =
    []

  method doCloseAttack (player : Player.player) : Gameobj.gameobj list =
    []

  method stopCloseAttack (player : Player.player) : Gameobj.gameobj list =
    []

  method doFarAttack (player : Player.player) : Gameobj.gameobj list =
    []

  method stopFarAttack (player : Player.player) : Gameobj.gameobj list =
    []

end;;

class soulPower cfgfile =
object (self)
  inherit power cfgfile

  method doJump (player : Player.player) : Gameobj.gameobj list =
    []

  method stopJump (player : Player.player) : Gameobj.gameobj list =
    []

  method doDefend (player : Player.player) : Gameobj.gameobj list =
    []

  method stopDefend (player : Player.player) : Gameobj.gameobj list =
    []

  method doCloseAttack (player : Player.player) : Gameobj.gameobj list =
    []

  method stopCloseAttack (player : Player.player) : Gameobj.gameobj list =
    []

  method doFarAttack (player : Player.player) : Gameobj.gameobj list =
    []

  method stopFarAttack (player : Player.player) : Gameobj.gameobj list =
    []

end;;

class starPower cfgfile =
object (self)
  inherit power cfgfile

      method doJump (player : Player.player) : Gameobj.gameobj list =
    []

  method stopJump (player : Player.player) : Gameobj.gameobj list =
    []

  method doDefend (player : Player.player) : Gameobj.gameobj list =
    []

  method stopDefend (player : Player.player) : Gameobj.gameobj list =
    []

  method doCloseAttack (player : Player.player) : Gameobj.gameobj list =
    []

  method stopCloseAttack (player : Player.player) : Gameobj.gameobj list =
    []

  method doFarAttack (player : Player.player) : Gameobj.gameobj list =
    []

  method stopFarAttack (player : Player.player) : Gameobj.gameobj list =
    []

end;;

class flamePower cfgfile =
object (self)
  inherit power cfgfile

  method doJump (player : Player.player) : Gameobj.gameobj list =
    []

  method stopJump (player : Player.player) : Gameobj.gameobj list =
    []

  method doDefend (player : Player.player) : Gameobj.gameobj list =
    []

  method stopDefend (player : Player.player) : Gameobj.gameobj list =
    []

  method doCloseAttack (player : Player.player) : Gameobj.gameobj list =
    []

  method stopCloseAttack (player : Player.player) : Gameobj.gameobj list =
    []

  method doFarAttack (player : Player.player) : Gameobj.gameobj list =
    []

  method stopFarAttack (player : Player.player) : Gameobj.gameobj list =
    []

end;;

class waterPower cfgfile =
object (self)
  inherit power cfgfile

  method doJump (player : Player.player) : Gameobj.gameobj list =
    []

  method stopJump (player : Player.player) : Gameobj.gameobj list =
    []

  method doDefend (player : Player.player) : Gameobj.gameobj list =
    []

  method stopDefend (player : Player.player) : Gameobj.gameobj list =
    []

  method doCloseAttack (player : Player.player) : Gameobj.gameobj list =
    []

  method stopCloseAttack (player : Player.player) : Gameobj.gameobj list =
    []

  method doFarAttack (player : Player.player) : Gameobj.gameobj list =
    []

  method stopFarAttack (player : Player.player) : Gameobj.gameobj list =
    []

end;;

class airPower cfgfile =
object (self)
  inherit power cfgfile

  (* Okay, this is weird.  And it should probably just be a jet-pack. 
     Basically, at random times, you don't jump when you should.
  *)
  method doJump (player : Player.player) : Gameobj.gameobj list =
    let jump () = 

      player#setOnGround false;
      player#getVector#addxy 0. jumpPower;
    in

    let airJump () = 
      player#getVector#sety (-.10.);
    in

      if player#isOnGround then (
	  if cooldownTimer <= 0 && player#getEnergy >= jumpEnergy then (
	      player#doWithEnergyCost jump jumpEnergy;
	      cooldownTimer <- jumpCool;
	    )
	  else
	    ();
	)
      else
	if (cooldownTimer <= 0) && 
	  (player#getVector#gety < 0.) then (
	    player#doWithEnergyCost airJump (jumpEnergy /. 5.);
	  );


	  (* Randomize particle position and speed a bit, then add it *)
(*	  let srcx = player#getX +. (player#getW /. 2.)
	  and srcy = player#getY -. player#getH in
	    for i = 0 to 30 do
	      let p = new Particles.particle "AirMistParticle.cfg" 
		player#getX player#getY in
		p#setXY srcx srcy;
		let h = if (Random.int 2) = 1 then -8. +. Random.float 5.
		  else 8. -. Random.float 5. in
		  p#getVector#addxy h (-.(Random.float 8.));
		  Particles.particles#add p;
	    done;
*)


      []

  method stopJump (player : Player.player) : Gameobj.gameobj list =
    []

  (* XXX: You can keep this on indefinately.
     What I'd LIKE to do is, say, tap it and let it last 5 seconds or something
     and only use up energy once.  Because with any regen at ALL they're
     always going to have more than the required amount of energy...
     That implies mechanisms to have a steady effect in progress.
     Hm.
  *)
  method doDefend (player : Player.player) : Gameobj.gameobj list =
    if (player#getEnergy >= defendEnergy) && cooldownTimer <= 0 then (
	for i = 0 to 10 do
	  let locx = player#getX -. 5. +. (Random.float (player#getW +. 10.))
	  and locy = player#getY +. 5. -. (Random.float (player#getH +. 10.)) in
	    Particles.particles#make "AirMistParticle.cfg" locx locy;
	    player#removeEnergy defendEnergy;
	    cooldownTimer <- defendCool;
	    player#getArea#setTangible false;
	done
      )
    else
      ();
    []

  method stopDefend (player : Player.player) : Gameobj.gameobj list =
    player#getArea#setTangible true;
    []

  method doCloseAttack (player : Player.player) : Gameobj.gameobj list =
    []

  method stopCloseAttack (player : Player.player) : Gameobj.gameobj list =
    []

  method doFarAttack (player : Player.player) : Gameobj.gameobj list =
    []

  method stopFarAttack (player : Player.player) : Gameobj.gameobj list =
    []

end;;

class earthPower cfgfile =
object (self)
  inherit power cfgfile

  method doJump (player : Player.player) : Gameobj.gameobj list =
    []

  method stopJump (player : Player.player) : Gameobj.gameobj list =
    []

  method doDefend (player : Player.player) : Gameobj.gameobj list =
    []

  method stopDefend (player : Player.player) : Gameobj.gameobj list =
    []

  method doCloseAttack (player : Player.player) : Gameobj.gameobj list =
    []

  method stopCloseAttack (player : Player.player) : Gameobj.gameobj list =
    []

  method doFarAttack (player : Player.player) : Gameobj.gameobj list =
    []

  method stopFarAttack (player : Player.player) : Gameobj.gameobj list =
    []

end;;
