(* level.ml
   Contains all the things we need for a level.  Map, music, background image,
   bad guys, secrets, etc.

   Also has functions to actually load the level.  Yeesh, what kinda format
   are we going to have?
   Probably a list of points, or something.

   Okay, lessee...
   Universal things: Connections between levels, enemy difficulty
   Domain-level things: Color scheme, background, bits avaliable
   Local things: Bit selection and placement, drawing etc, triggers and
   special things,
   
   Maybe a level has a "domain" property that is given to it?

   How do we make levels?  Well, we load in all the possible level
   basises.  Then we select X of them, where X is levels-per-cluster * 
   number-of-clusters.  Then we take levels-per-cluster levels and network
   'em together, then stick in a the exit level, then sprinkle the 
   entry point and connections to other clusters.
   

   Simon Heath
   14/7/2006
*)

open Gameobj;;
open Util;;
open Area;;


(* Each cluster has a color scheme, a list of possible room-layouts,
   a list of possible room-features (config file lists kinda suck but work),
   a list of possible enemies, the generated rooms, a core room and boss, 
   an element designator (aka various arbitrary flags and triggers)

   Okay, so if the cluster stores the rooms itself, then 


   Wait.  Say we have a list of all levels, with numbers.
   Then each level has a list of other levels it connects to.
   The only vaguely complicated operation is adding a new link, and
   that only happens once.
   Hmmm.
*)


type cluster =
    Start
  | End
  | Soul
  | Star
  | Flame
  | Water
  | Earth
  | Air

;;


(* Grrr... we DO need a way to read a cluster config files, so it's
   not rediculous to tweak colors and music and such.
   Oh, and maps
*)

class clusterProps cfgfile =
  let cfg = Resources.getConfig cfgfile in
object (self)
  val name = cfg#getStr "name"
  val color = ((cfg#getInt "color" "r"), (cfg#getInt "color" "g"),
	       (cfg#getInt "color" "b"))

  val roomMaps = cfg#getStrList "cluster" "roomMaps"

  val numRooms = cfg#getInt "cluster" "numRooms"

  val background = cfg#getStr "cluster" "background"


  method getBackground =
    background


  method getColor =
    color

  method getName =
    name

  method getNumRooms =
    numRooms

  method getMaps =
    roomMaps

  method getMap n =
    List.nth roomMaps n

  method getRandomMap =
    let n = Random.int (List.length roomMaps) in
      self#getMap n

end;;


(* Kinda janky, but they're not gonna change... *)
let startCluster = new clusterProps "start.cls"
and endCluster   = new clusterProps "end.cls"
and soulCluster  = new clusterProps "soul.cls"
and starCluster  = new clusterProps "star.cls"
and flameCluster = new clusterProps "flame.cls"
and waterCluster = new clusterProps "water.cls"
and earthCluster = new clusterProps "earth.cls"
and airCluster   = new clusterProps "air.cls"
;;





let roomsPerCluster = 8;;
let numClusters = 8;;

(* Each cluster has eight rooms, indexed *)
let getCluster n =
  if n < (0 * roomsPerCluster) then
	    Util.error "Impossible cluster index: %d\n" n
  else if n < (1 * roomsPerCluster) then
    startCluster
  else if n < (2 * roomsPerCluster) then
    endCluster
  else if n < (3 * roomsPerCluster) then
    soulCluster
  else if n < (4 * roomsPerCluster) then
    starCluster
  else if n < (5 * roomsPerCluster) then
    flameCluster
  else if n < (6 * roomsPerCluster) then
    waterCluster
  else if n < (7 * roomsPerCluster) then
    earthCluster
  else if n < (8 * roomsPerCluster) then
    airCluster
  else
    Util.error "Impossible cluster index: %d\n" n
;;



let doorsize = 50.;;


class door (fromr : int) (tor : int) =
object (self)
  val mutable fromRoom = fromr
  val mutable toRoom = tor
  val mutable x = 0.
  val mutable y = 0.

  method setX nx = 
    x <- nx

  method setY ny =
    y <- ny

  method moveTo nx ny =
    x <- nx;
    y <- ny

  method getX =
    x

  method getY =
    y

  method getFrom =
    fromRoom

  method getTo =
    toRoom
(*
  method drawGlyphZero scr color x y =
    ()

  method drawGlyphOne scr color x y =
    ()

  method drawGlyphTwo scr color x y =
    ()

  method drawGlyphThree scr color x y =
    ()

  method drawGlyphFour scr color x y =
    ()

  method drawGlyphFive scr color x y =
    ()

  method drawGlyphSix scr color x y =
    ()

  method drawGlyphSeven scr color x y =
    ()
*)
  (* XXX: Finish this *)
  method drawRoomGlyph scr destCluster =
(*    let num = toRoom mod 8 in*)
    let color = destCluster#getColor in
    let nx = (x2screen x !logscreenx)
    and ny = (y2screen y !logscreeny)
    and w = 30
    and h = 30 in
      Drawing.drawRect scr color nx ny w h


  method draw scr =
    let destCluster = getCluster toRoom in
    let nx = (x2screen x !logscreenx)
    and ny = (y2screen y !logscreeny)
    and w = (int_of_float doorsize)
    and h = (int_of_float doorsize) in
      self#drawRoomGlyph scr destCluster;
      Drawing.drawRect scr (255,255,255) nx ny w h;
      Drawing.drawRect scr destCluster#getColor (nx+1) (ny+1) (w-2) (h-2)


  (* Let's just use a bounding circle here... *)
  method isOn (gameobj : gameobj) =
    let gx = gameobj#getX
    and gy = gameobj#getY in
    let dx = x -. gx
    and dy = y -. gy in
      ((Util.fabs dx) < doorsize /. 2.) && ((Util.fabs dy) < doorsize /. 2.)
end;;








(* So, should rooms hang onto the screen and
   game objects?  Um, I'm actually not too sure...
   In fact, no.

*)
class room idx =
  let c = getCluster idx in
object (self)

  val index = idx

  val cluster = c

  val color = c#getColor

  val mutable lines = Line.file2Lines c#getRandomMap

  val mutable doors : door list = []

  method print =
    Printf.printf "%d" index

  method getIndex =
    index

  (* Optimize line-drawing! *)
  method drawLevel scr =
    List.iter (fun x -> Line.drawLine scr cluster#getColor x) lines;
    List.iter (fun x -> x#draw scr) doors;
(*
    let rec loop lst count = 
      if count = 0 then
	()
      else (
	let area = List.hd lst in
	let nx = (x2screen area#getx !logscreenx)
	and ny = (y2screen area#gety !logscreeny)
	and w = (int_of_float area#getw)
	and h = (int_of_float area#geth) in
	  Drawing.drawRect scr (255,0,0) nx ny w h;
	  loop (List.tl lst) (count - 1)
      )
    in
      loop doors (List.length exits);
*)
      Drawing.drawRect scr c#getColor 0 0 799 599;




  method getLines =
    lines

  method setLines l =
    lines <- l

  method addLine line =
    lines <- line :: lines

  method setLine newlines =
    lines <- newlines

  method collideLevel (obj : gameobj) t =
    obj#setOnGround false;
    List.iter (fun x -> obj#doTerrainCollision x t) lines

  method getCluster =
    cluster


  method addDoor (door : door) : unit = 
    if not (List.mem door doors) then
      doors <- door :: doors;
	

  method getExit (n : int) : door =
    List.nth doors n


  method isOnExit (obj : gameobj) =
    let rec loop = function
	[] -> None
      | hd :: tl -> 
	  if hd#isOn obj then
	    Some hd
	  else
	    loop tl
    in
      loop doors


    
end;;



let makeWorld () =
  Array.init (numClusters * roomsPerCluster) (fun x -> new room x) 
;;


(* This connects rooms to each other in a very simple manner *)
let makeDoors world =
  let connectRoom room =
    let i = room#getIndex in 
      if i < ((numClusters * roomsPerCluster) - 1) then (
	let d = new door i (i+1) in
	  d#moveTo 100. 50.;
	  room#addDoor d;
      );
      if i > 0 then (
	let d = new door i (i-1) in
	  d#moveTo 500. 50.;
	  room#addDoor d;
      );
  in

  Array.iter connectRoom world
;;





