(* background.ml
   This handles the background-drawing functions and such.

   It would be cool if we could scroll the background algorithmically,
   but the fact that it generally depends on the state of the buffer
   kinda screws that.  What we should do instead, if we want scrolling,
   is to not blit the buffer to the screen at once, but blit it in
   1-4 chunks, depending on the offset.

   Simon Heath
   15/10/2006
*)

open Sdlvideo;;


let togglePixel surf x y color =
(*  let a, b, c = get_pixel_color surf ~x: x ~y: y in
  Printf.printf "P: %d, %d, %d\n" a b c; *)

  (* This won't be exactly the same with 16 bit color...  Hm. *)
  if (get_pixel_color surf ~x: x ~y: y) = (0,0,0) then
    put_pixel_color surf ~x: x ~y: y color
  else
    put_pixel_color surf ~x: x ~y: y (0,0,0)
;;


(* Munching squares! 
   It works well as it is.  If you change colors based on t, it gets cooler,
   but too much and it's hard to see the actual game.
   Clearing the screen and putting pixels instead of toggling them
   is a bit TOO subdued, unless we make the colors bright.

   XXX: It'd be HOT to make the background-pattern slowly grow faster as we
   went on in the level...!
*)
let munch surf t = 
(*  let t = t / 11 in  *)
  let color = ((32 + (t mod 100)),(32+(t mod 100)),(32+(t mod 100))) in  (* Change these colors based on t! *)

    for x = 0 to !Util.screenx do
      let y = (x lxor t) mod !Util.screeny in
	togglePixel surf x y color;

    (*	togglePixel surf ((x - (int_of_float !Util.logscreenx)) mod !Util.screenx) y color *)
    done
;;



let fillColor surf col =
   fill_rect ~rect: (surface_info surf).clip_rect surf (map_RGB surf col)
;;


class background scr mode =
  let sinfo = surface_info scr in
object (self)

  val screen = scr
  val buffer = create_RGB_surface_format scr [] ~w: sinfo.w ~h: sinfo.h

  val mutable mode = mode
  val mutable modefunc = munch

  initializer
    fillColor screen black;  
    self#switchMode mode


  method switchMode (m : string) : unit =
    mode <- m;
    match mode with
	"munch" -> (modefunc <- munch);
      | _ -> (Util.error "ERROR: Invalid background mode: %s\n" mode);




  method getMode =
    mode



  method update t =
    if must_lock buffer then
      lock buffer;

    modefunc buffer t;

    if must_lock buffer then
      unlock buffer;
    (*    fillColor buffer (t, 0, 255-5);   Kinda cool, needs work *)
    blit_surface ~src: buffer ~dst: screen ()

end;;
