(* main.ml
   Main game logic and setup.
 
   Simon Heath
   11/3/2005
*)


open Config;;
open Sdlvideo;;
open Sdlkey;;
open Util;;
open Line;;


(*  This should be able to work on any gameobj. *)
let centerScreenOn (o : Gameobj.gameobj) =
  logscreenx := o#getArea#getx;
  logscreeny := o#getArea#gety;
;;

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




let fillColor surf col =
   fill_rect ~rect: (surface_info surf).clip_rect surf (map_RGB surf col)
;;

(* Munching squares! 
   Make a real background system, mook.
   One with a seperate buffer.
*)
let background surf t = 
  let color = (255,0,0) in
    for x = 0 to !Util.screenx do
      let y = (x lxor t) mod !Util.screeny in
	put_pixel_color surf ~x: x ~y: y color;
    done
;;
    


let mainloop scr bkg =

(*  Audio.playMusic "testy.ogg"; *)

  let lastupdate = ref (Sdltimer.get_ticks ()) in
  let updatetime = ref (Sdltimer.get_ticks ()) in
  let framecount = ref 0 in
  let continue = ref true in

  let doordelay = ref 0 in

  let rooms = Level.buildCluster "StartCluster.cfg" in
  let currentRoom = ref (List.hd rooms) in

  let p = new Player.player "player.cfg" in
  let maxdt = ref 0 in

    logscreenx := 368.; 
    logscreeny := 200.;  
    (* MAINLOOP! *)
    while !continue do

      (* Prologue *)
      incr framecount;

      let dt = (Sdltimer.get_ticks ()) - !lastupdate in
	maxdt := max dt !maxdt;

	(* Event handling *)      
	ignore (Sdlevent.poll());
	if (is_key_pressed !Input.menu) or (is_key_pressed KEY_q) then
	  continue := false;

	if (is_key_pressed !Input.goright) then
	  p#doWalk Player.right;
	if (is_key_pressed !Input.goleft) then
	  p#doWalk Player.left;
	if (is_key_pressed !Input.jump) then
	  p#doJump;
	if (is_key_pressed !Input.brake) then
	  p#doBrake;


	if (is_key_pressed !Input.action) then (
	  if ((Sdltimer.get_ticks ()) - !doordelay) > 200 then ( 
	    doordelay := Sdltimer.get_ticks ();
	    let ext = !currentRoom#isOnExit (p :> Gameobj.gameobj) in
	      match ext with
		  None -> ()
		| Some( room ) ->
		    Printf.printf "Going from room %d to %d\n" 
		      !currentRoom#getIndex
		      room#getIndex;
		    currentRoom := room;
	  )
	);


	
	(* Calculating 
	   We must do INPUT, then CALCULATE, then do COLLISION,
	   then do DRAWING.  In that order!
	*)
	p#doGravity dt;
	p#doFriction 0.99;

	let dt = (Sdltimer.get_ticks ()) - !lastupdate in
	  p#calculate dt;
	  Particles.particles#calc dt;

	  !currentRoom#collideLevel (p :> Gameobj.gameobj);


	  (*	o1#calculate 1;
		o2#calculate 2; *)


	  looselyCenterScreenOn (p :> Gameobj.gameobj);

	  (* Blittage *)
	  fillColor scr black;  
	  background scr !framecount;

	  Drawing.drawLine scr red (-5) (-200) 200 5;
	  


	  (* Okay.  So, the objects in the level work right,
	     but the tile level background itself doesn't move according to
	     screenworld location
	  *)

	  !currentRoom#drawLevel scr;

	  (* 	Drawing.drawLine scr (255,0,0) 001 400 300 350;  *)
	  (*	Drawing.drawCircle scr (0,0,255) 200 200 10; *)
	  


	  p#draw scr dt;
	  Particles.particles#draw scr dt;



	  (* Draw GUI.  Sorta. *)
	  Text.drawTextAbs "Foo!" "Arial.ttf" red 300 0 scr;
	  Text.drawTextAbs 
	    (Printf.sprintf 
	       "lastupdate: %d updatetime: %d fps: %2.2f dt: %d ms" 
	       (!lastupdate) (!updatetime) 
	       (1000. /. (float_of_int (Sdltimer.get_ticks () / !framecount))) dt)
	    "Arial.ttf" red 100 20 scr;


	  (*
	    let timediff = (!updatetime - !lastupdate) in
	    if timediff < 40 then
	    Sdltimer.delay (40 - timediff);
	  *)



	  (* Epilogue *)
	  flush stdout;
	  flip scr;
	  (* Um, this /might/ prevent lag from happening during GC... 
	     But not really.
	  *)
	  (*	    Gc.full_major ();  *)

	  (* Timing *)

	  lastupdate := !updatetime;
	  updatetime := Sdltimer.get_ticks ();
	  

    done;

    Printf.printf "Max dt: %d\n" !maxdt;
;;


let main () =

  (* Init... messing up the order of things, esp. config-files, could be bad
  *)
  Sdl.init [`EVERYTHING];
  Sdlwm.set_caption ~title: "Starmaze" ~icon: "None";
  Sdlmouse.show_cursor false;

  let l1 = Line.ALine( 0., 0., 10., 10. )
  and l2 = Line.ALine( 0., 10., 10., 0. ) in
  let b1, _, _ = Line.linesColliding l1 l2 
  and b2, _, _ = Line.linesColliding l2 l1 in
    if b1 then
      print_endline "Foo!"
    else
      print_endline "Bar!";
    if b2 then
      print_endline "Foo2!"
    else
      print_endline "Bar2!";


  Sdlttf.init ();
  Audio.initSound 16; 
  Random.self_init ();

  Input.loadKeyDefs ();

  Printf.printf "Sha: %d, %d\n" !screenx !screeny;

  (* Grafix setup *)
  let screen = set_video_mode ~w: !screenx ~h: !screeny ~bpp: 16 
    [`DOUBLEBUF; `SWSURFACE] in
  

  let bkg = create_RGB_surface_format screen [`HWSURFACE] ~w: !screenx ~h: !screeny in

    fill_rect bkg (map_RGB bkg black);
  

    (*set_color_key screen (get_pixel screen ~x: 0 ~y: 0); *)

    (* Mainloop *)

    mainloop screen bkg;

(*    while true do () done; *)


    (* De-init *)
    Sdlttf.quit ();
    Sdl.quit ()    
;;


let _ =
  main ();;

