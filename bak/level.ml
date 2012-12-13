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


let doors = [(new area "exit1.cfg"); (new area "exit2.cfg"); 
	     (new area "exit3.cfg"); (new area "exit4.cfg"); 
	     (new area "exit5.cfg")]


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



class room (clustr : cluster) (idx : int) =
object (self)

  val cluster = clustr

  val index = idx

  val mutable lines = Line.file2Lines (clustr#getRandomRoomMap)

  val mutable exits : room list = []

  method print =
    Printf.printf "%d" index

  method getIndex =
    index

  method drawLevel scr =
    List.iter (fun x -> Line.drawLine scr cluster#getColor x) lines;
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
      Drawing.drawRect scr (128,128,0) 0 0 799 599;




  method getLines =
    lines

  method setLines l =
    lines <- l

  method addLine line =
    lines <- line :: lines

  method setLine newlines =
    lines <- newlines

  method collideLevel (obj : gameobj) =
    obj#setOnGround false;
    List.iter (fun x -> obj#doTerrainCollision x) lines

  method getCluster =
    cluster


  method addExit (room : room) : unit = 
    if not (List.mem room exits) then
      exits <- room :: exits;
	

  method getExit (n : int) : room =
    List.nth exits n


  method isOnExit (obj : gameobj) =
    let rec loop idx = function
	[] -> None
      | hd :: tl -> 
	  let area = List.nth doors idx in
	    if area#isColliding obj#getArea then
	      Some hd
	    else
	      loop (idx + 1) tl
    in
      loop 0 exits
      
    
    
end 


(* Okay... under the new system (again), we'd first build a list of rooms.
   Then each room would just refer directly to other rooms...
*)


and cluster cfgfile =
  let cfg = Resources.getConfig cfgfile in
object (self)
  val name = cfg#getStr "cluster" "name"
  val color = ((cfg#getInt "color" "r"), (cfg#getInt "color" "g"),
	       (cfg#getInt "color" "b"))

  val roomMaps = cfg#getStrList "cluster" "roomMaps"

  val numRooms = cfg#getInt "cluster" "numRooms"


  method getColor =
    color

  method getName =
    name

  method getNumRooms =
    numRooms

  method getRoomMaps =
    roomMaps

  method getRoomMap n =
    List.nth roomMaps n

  method getRandomRoomMap =
    let n = Random.int (List.length roomMaps) in
      self#getRoomMap n

end;;





(* Okay.  Here's an experimental algorithm for how rooms are connected
   to each other: 

   KISS.  Each room connects to the next, and in doing so, is connected
   to the previous too.
*)
let buildCluster cfgfile =
  let c = new cluster cfgfile in
  let rec initRooms accm roomsLeft =
    if roomsLeft = 0 then
      accm
    else (
      let r = new room c roomsLeft in
	if accm <> [] then (
	  let nxt = List.hd accm in
	    r#addExit nxt;
	    nxt#addExit r;
	);
	  initRooms (r :: accm) (roomsLeft - 1)
    )
  in
    (initRooms [] (c#getNumRooms - 1));
;;
