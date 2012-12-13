(* particles.ml
   Particle system.
   Particles would be: smoke, flame, shots maybe, snow, rain, clouds/fog?,
   drifting/floating anything...
   They generally don't interact.  Just there to look pretty.

   o/~ Particle man, particle man, doin' the things a particle can... o/~

   Simon Heath
   30/7/2006
*)


open Util;;
open Resources;;


(* We need an option to make particles not draw until their life
   hits a certain point,
   one to make 'em flicker on an off more or less at random,
   one to make 'em pulse regularly,
   one to make 'em fade once their life reaches a certain point,

   It'd be nice to be able to specify starting vector and such per-
   particle...

*)
type options =
    FLICKER
  | PULSE
  | SPIRAL
  | FADE
;;

let str2option = function
    "flicker" -> FLICKER
  | "pulse" -> PULSE
  | "spiral" -> SPIRAL
  | "fade" -> FADE
  | _ -> error "Invalid option for particle! %s" ""
;;



    

let loadColorList cfg =
  let redList = cfg#getIntList "particle" "colorR"
  and blueList = cfg#getIntList "particle" "colorB"
  and greenList = cfg#getIntList "particle" "colorG" in
  let rec loop accm r b g =
    if r = [] then
      accm
    else
      let rd = List.hd r
      and bl = List.hd b
      and gr = List.hd g in
	loop ((rd,gr,bl) :: accm) (List.tl r) (List.tl b) (List.tl g)
  in
    if ((List.length redList) = (List.length blueList)) &&
      (List.length blueList) = (List.length greenList) then
	loop [] redList blueList greenList
    else
      raise (Config.Config_error "Particle color lists need to be the same length!")
;;


let drawNothing scr =
  ()
;;


(* Baka.  Instead of subclassing 'till the cows come home,
   just make options from the config file.
   How many types of particle are we gonna need, anyway?  Probably
   not THAT many.

   XXX: Hmmm.  Every time we make a new particle, we load all the colors
   again.  Maybe we can cache a particle instance, then just clone it
   off?  Oh well, doesn't matter at the moment.
   While we're at it, make a freelist for particles instead of leaving
   it all up to the poor GC.
   Weak pointers could work well, I bet. (No, they'd defeat the purpose)
   We can do both in the same createParticle function.
   Then there can be a removeParticle function, I suppose...
   Eh, all particles can just call it when they die.
   The particle system can hang on to it, perhaps?  Not sure it needs to.
*)
class particle cfgfile px py =
  let cfg = getConfig cfgfile in
object (self)
  inherit Gameobj.gameobj cfgfile

  val mutable options = List.map str2option (cfg#getStrList "particle" "options")
    (* You know, OCaml makes "self" rather bleedin' useless. *)
  val mutable drawFunc = drawNothing

  val mutable colors = Array.of_list (loadColorList cfg)
  val mutable currentColor = 0
  val mutable colorShift = cfg#getInt "particle" "colorShift"
  val mutable colorShiftCounter = 0

  initializer
    self#setShape (cfg#getStr "particle" "shape");
    area#setxy px py;


  (* Other shapes... fuzzy circle? *)
  method setShape = function
      "circle" -> drawFunc <- self#drawCircle
    | "cross" -> drawFunc <- self#drawCross
    | "square" -> drawFunc <- self#drawSquare
    | "spark" -> drawFunc <- self#drawSpark
    | "mist" -> drawFunc <- self#drawMist
    | x -> error "Invalid shape for particle: %s" x


  method behavior (dt : int) =
    self#doGravity dt;
    self#damage Gameobj.Start (float_of_int dt);

    (* colorShift <= 0 means no color changing. *)
    if colorShift > 0 then (
      colorShiftCounter <- colorShiftCounter - dt;
      if colorShiftCounter < 0 then (
	colorShiftCounter <- colorShiftCounter + colorShift;
	currentColor <- (currentColor + 1) mod (Array.length colors)
      )
    );

    
(* Hmmm, it's a start... Fade-rate?*)

    let fade color =
      let r, g, b = color in
	(max (r-25) 0, max (g-25) 0, max (b-25) 0)
    in
    if List.mem FADE options then
       colors.(currentColor) <- fade colors.(currentColor)

	

(* Make these generic drawing funcs that just do logical coord stuff? *)
  method drawCircle scr = 
    let nx = x2screen area#getx !logscreenx
    and ny = y2screen area#gety !logscreeny in
      Drawing.drawCircle scr colors.(currentColor) nx ny (int_of_float (area#getw))

  method drawCross scr =
    let nx = x2screen area#getx !logscreenx
    and ny = y2screen area#gety !logscreeny in
      Drawing.drawLine scr colors.(currentColor) 
	(nx - (int_of_float area#getw)) 
	(nx + (int_of_float area#getw))
	ny ny;

      Drawing.drawLine scr colors.(currentColor) 
	nx nx
	(ny - (int_of_float area#geth))
	(ny + (int_of_float area#geth));

  method drawMist scr =
    let nx = x2screen area#getx !logscreenx
    and ny = y2screen area#gety !logscreeny in
      for i = 0 to 20 do
	let x = nx + (Random.int (int_of_float area#getw))
	and y = ny + (Random.int (int_of_float area#geth)) in
	Drawing.drawPoint scr colors.(currentColor) x y
      done;

      

  method drawSquare scr = 
    let nx = x2screen area#getx !logscreenx
    and ny = y2screen area#gety !logscreeny in
      Drawing.drawRect scr colors.(currentColor)
	nx ny (int_of_float area#getw) (int_of_float area#geth)


  (* Should really be a *-shaped thing or something. *)
  method drawSpark scr = 
    self#drawCircle scr;
    self#drawCross scr;

  method draw scr =
    (*    let nx = x2screen area#getx !logscreenx
	  and ny = y2screen area#gety !logscreeny in
    *)
    drawFunc scr;


end;;


class particleSystem =
  
object (self)
  val mutable particles = []

  method add (p : particle) =
    particles <- p :: particles

  method make cfg x y =
    self#add (new particle cfg x y)

  method clear =
    particles = []
    

  method calc dt =
    particles <- 
      List.filter (fun x -> x#calculate dt; x#behavior dt; x#isAlive) particles


  method draw scr =
    List.iter (fun x -> x#draw scr) particles;

end;;


let particles = new particleSystem;;
