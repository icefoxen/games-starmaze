(* driver.ml
   This is the class that actually does the mainloop stuff.
   It is completely seperate from rooms and such.
*)


open Util;;
open Sdlkey;;
open Input;;


(* Unportable as hell (in terms of screen resolution, I mean),
   but it works!
*)
let looselyCenterScreenOn (o :Gameobj.gameobj) =
  let objx = o#getArea#getx
  and objy = o#getArea#gety in

  let offset = 150. in
    if ((objx -. !logscreenx) < -.offset) then (
      logscreenx := !logscreenx +. (objx -. !logscreenx +. offset)
    )
    else if ((objx -. !logscreenx) > offset) then (
      logscreenx := !logscreenx +. (objx -. !logscreenx -. offset)
    );

    if ((objy -. !logscreeny) < -.offset) then (
      logscreeny := !logscreeny +. (objy -. !logscreeny +. offset)
    )
    else if ((objy -. !logscreeny) > offset) then (
      logscreeny := !logscreeny +. (objy -. !logscreeny -. offset)
    )
;;



class driver scr =
  let w = Level.makeWorld () in
object (self)
  val world = w
  val mutable currentRoom = w.(0)
  val mutable player = new Player.player "player.cfg"
  val mutable objects : Gameobj.gameobj list = []
  val mutable background = new Background.background scr "munch"

  val mutable currentMusic = ""

  (* I'm a bit leery of making timing information global, but... *)
  val mutable framecount = 0
  val mutable lastframe = 0
  val mutable framestarttime = 0
  val mutable dt = 0
  val mutable maxdt = 0

  val mutable continue = true

  val  currentInput = Input.newKeystate ();
  val lastInput = Input.newKeystate ();

  (* I still feel like this is a kludge.  Timers! *)
  val mutable doordelay = 0


  (* Some meta-player info that's probably best put here... *)
  val mutable currentPower = new Power.airPower "air.pow";



  initializer
    Level.makeDoors w;
    self#addObject (new Gameobj.gameobj "Target.cfg");
    self#addObject (player :> Gameobj.gameobj);
    currentPower <- new Power.startPower "start.pow";

  method drawLevel =
    currentRoom#drawLevel scr

  method drawObjects : unit =
    List.iter (fun x -> x#draw scr) objects;


  method drawGui =
(*    Text.drawTextAbs 
      (Printf.sprintf 
	 "lastframe: %d framestarttime: %d fps: %2.2f dt: %d ms" 
	 lastframe framestarttime 
	 (1000. /. (float_of_int (Sdltimer.get_ticks () / framecount))) dt)
      "cour.ttf" (255,0,0) 100 20 scr;
*)
    (* Life and energy bars *)
    Drawing.drawFilledRect scr 5 5 (int_of_float (player#getHits)) 10 (255,0,0);
    Drawing.drawFilledRect scr 5 20 (int_of_float (player#getEnergy))
      10 (0,0,255);

  method draw =
    looselyCenterScreenOn (player :> Gameobj.gameobj);

    background#update framecount;

    self#drawLevel;
    self#drawObjects;
    Particles.particles#draw scr;
    self#drawGui;

  method receiveInput = 
    ignore (Sdlevent.poll());
    if (is_key_pressed !Input.menu) or (is_key_pressed KEY_q) then
      self#stopGame;
    
    if (is_key_pressed !Input.goright) then
      currentInput.k_right <- true;
    if (is_key_pressed !Input.goleft) then
      currentInput.k_left <- true;
    if (is_key_pressed !Input.brake) then
      currentInput.k_brake <- true;

    if (is_key_pressed !Input.jump) then
      currentInput.k_jump <- true;
    if (is_key_pressed !Input.closeattack) then
      currentInput.k_close <- true;
    if (is_key_pressed !Input.farattack) then
      currentInput.k_far <- true;
    if (is_key_pressed !Input.defend) then
      currentInput.k_defend <- true;

    if (is_key_pressed !Input.action) then
      currentInput.k_action <- true;


  method doInput =
    self#receiveInput;

    if currentInput.k_right then
      player#doWalk right;
    if currentInput.k_left then
      player#doWalk left;
    if currentInput.k_brake then
      player#doBrake;

    if currentInput.k_jump then
      self#addObjects (currentPower#doJump player)
    else if lastInput.k_jump then
      self#addObjects (currentPower#stopJump player);
    if currentInput.k_close then
      self#addObjects (currentPower#doCloseAttack player);
    if currentInput.k_far then
      self#addObjects (currentPower#doFarAttack player);
    if currentInput.k_defend then
      self#addObjects (currentPower#doDefend player)
    else if lastInput.k_defend then
      self#addObjects (currentPower#stopDefend player);


    if currentInput.k_action then
      if ((Sdltimer.get_ticks ()) - doordelay) > 200 then ( 
	doordelay <- Sdltimer.get_ticks ();
	let ext = currentRoom#isOnExit (player :> Gameobj.gameobj) in
	  match ext with
	      None -> ()
	    | Some( door ) ->
		self#switchRoom door#getTo;
      );
    
    mirrorKeystate currentInput lastInput;
    clearKeystate currentInput;


  method doTerrainCollide =
    List.iter (fun x -> currentRoom#collideLevel x dt) objects;
  (* area#isColliding might work.  Ooo, see if you can make things
     push-able...
     Make a collide method for gameobj, so that shot and enemy and 
     stuff can override it.
  *)
  method doObjectCollide =
    let rec collideEverything lst =
      if lst = [] then
	()
      else (
	  let a = List.hd lst
	  and b = List.tl lst in
	    List.iter (fun x -> a#collideWithObject x;) b;
	    collideEverything b;
	)
    in
      collideEverything objects;
	


  method updateTimers =
    lastframe <- framestarttime;
    framestarttime <- Sdltimer.get_ticks ();
    framecount <- framecount + 1;

    (* Max timestep is 75ms.  Any longer and things can fall through
       things. *)
    dt <- min 75 (framestarttime - lastframe);
    maxdt <- max dt maxdt;
    
  method calcObjects =
    objects <- List.filter (fun x -> x#calculate dt; x#isAlive) objects;

  method calculate =
    self#calcObjects;
    currentPower#calc dt;
    Particles.particles#calc dt;



  method mainLoop =
    while continue do

      self#updateTimers;
      self#doInput;
      self#calculate;
      self#doTerrainCollide;
      self#doObjectCollide;
      self#draw;

      flush stdout;
      Sdlvideo.flip scr;
    done;



  method addObject x =
    objects <- x :: objects

  method addObjects x =
    objects <- x @ objects

  method removeObject x =
    objects <- List.filter (fun itm -> itm <> x) objects

  method addParticles =
    ()

  method switchRoom i =
    Printf.printf "Going to room %d\n" i;
    currentRoom <- world.(i);


  method playMusic =
    ()



  (* This should eventually lead to a main menu or such, but for now
     just quits *)
  method stopGame =
    continue <- false;
    let seconds = (float_of_int lastframe) /. 1000. in
    let fps = (float_of_int framecount) /. seconds in
      Printf.printf "Avg FPS: %f\n" fps;
      Printf.printf "Max dt: %d\n" dt;

end;;
