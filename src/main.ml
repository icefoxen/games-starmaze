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



let main () =
  (* Init... messing up the order of things, esp. config-files, could be bad
  *)
  Sdl.init [`EVERYTHING];
  Sdlwm.set_caption ~title: "Starmaze" ~icon: "None";
  Sdlmouse.show_cursor false;


  Sdlttf.init (); 
(*  Audio.initSound 16;  *)
  Random.self_init ();

  Input.loadKeyDefs ();

  (* Graphics setup *)
  let screen = set_video_mode ~w: !screenx ~h: !screeny ~bpp: 16
    [`DOUBLEBUF; `SWSURFACE] in
  

  (* Mainloop *) 
  let game = new Driver.driver screen in
    game#mainLoop;

    (* De-init *)
    Sdlttf.quit ();
    Sdl.quit ()    
;;


let _ =
  main ();;
