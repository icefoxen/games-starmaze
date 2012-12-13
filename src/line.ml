(* A line!
   We use 'em in collision detection and terrain in general.

   To do: Diagonal lines, if necessary.

   Simon Heath
   4/7/2006
*)

(* A line is represented by the endpoints.  *)
type line =
  float * float * float * float
;;

let canonifyLines line =
let  x1, y1, x2, y2 = line in
      let a = y2 -. y1
      and b = x1 -. x2 in
      let c = (a *. x1) +. (b *. y1) in 
	(a, b, c)
;;


let isBetween thing lower upper =
  (thing >= lower) && (thing <= upper)
;;

(*  One of the most common tasks you will find in geometry problems is line intersection. Despite the fact that it is so common, a lot of coders still have trouble with it. The first question is, what form are we given our lines in, and what form would we like them in? Ideally, each of our lines will be in the form Ax+By=C, where A, B and C are the numbers which define the line. However, we are rarely given lines in this format, but we can easily generate such an equation from two points. Say we are given two different points, (x1, y1) and (x2, y2), and want to find A, B and C for the equation above. We can do so by setting 
A = y2-y1
B = x1-x2
C = A*x1+B*y1
Regardless of how the lines are specified, you should be able to generate two different points along the line, and then generate A, B and C. Now, lets say that you have lines, given by the equations:
A1x + B1y = C1
A2x + B2y = C2
To find the point at which the two lines intersect, we simply need to solve the two equations for the two unknowns, x and y. 
    double det = A1*B2 - A2*B1
    if(det == 0){
        //Lines are parallel
    }else{
        double x = (B2*C1 - B1*C2)/det
        double y = (A1*C2 - A2*C1)/det
    }
To see where this comes from, consider multiplying the top equation by B2, and the bottom equation by B1. This gives you
A1B2x + B1B2y = B2C1
A2B1x + B1B2y = B1C2
Now, subtract the bottom equation from the top equation to get
A1B2x - A2B1x = B2C1 - B1C2
Finally, divide both sides by A1B2 - A2B1, and you get the equation for x. The equation for y can be derived similarly. 

This gives you the location of the intersection of two lines, but what if you have line segments, not lines. In this case, you need to make sure that the point you found is on both of the line segments. If your line segment goes from (x1,y1) to (x2,y2), then to check if (x,y) is on that segment, you just need to check that min(x1,x2) ? x ? max(x1,x2), and do the same thing for y. You must be careful about double precision issues though. If your point is right on the edge of the segment, or if the segment is horizontal or vertical, a simple comparison might be problematic. In these cases, you can either do your comparisons with some tolerance, or else use a fraction class.

*)
let linesColliding line1 line2 =
  let xa1, ya1, xa2, ya2 = line1
  and xb1, yb1, xb2, yb2 = line2 in
  let a1, b1, c1 = canonifyLines line1
  and a2, b2, c2 = canonifyLines line2 in
  let det = (a1 *. b2) -. (a2 *. b1) in  (* Relative slope, I believe *)
    if det = 0.0 then
      (false, 0.0, 0.0) (* Lines are parallel *)
    else
      let x = ((b2 *. c1) -. (b1 *. c2)) /. det
      and y = ((a1 *. c2) -. (a2 *. c1)) /. det in
	if (isBetween x (min xa1 xa2) (max xa1 xa2)) &&
	  (isBetween y (min ya1 ya2) (max ya1 ya2)) &&
	  (isBetween x (min xb1 xb2) (max xb1 xb2)) &&
	  (isBetween y (min yb1 yb2) (max yb1 yb2)) then
	    (true, x, y)
	else
	  (false, 0.0, 0.0)
;;

let slope line = 
  let x1, y1, x2, y2 = line in
    (y2 -. y1) /. (x2 -. x1)
;;



(* Collides an area with a line.  Kinda sucky name, I know... *)
let collidingWithLine area line =
  let areaLines = area#toLines in
  let rec loop l accm =
    if accm = [] then
      false
    else
      let first = List.hd accm
      and rest = List.tl accm in
      let b, x, y = linesColliding l first in 
	if b then (
(*	  Printf.printf "X: %f Y: %f\n" x y; *)
	  true
	) else
	  loop l rest
  in
    loop line areaLines
;;


(* Returns < 0 for left of, 0 for right on (unlikely), and > 0 for right of. 

   XXX: THIS DOESN'T BLEEDIN' WORK if the slope of the incoming line
   is negative, or something close to that.
   Crap.
*)
let pointRelative line x y =
  let s = slope line 
  and x1, y1, x2, y2 = line in
  let line2 = (x1, y1, x, y) in
  let s2 = slope line2 in
  let ds = s -. s2 in
    if s > 0. then
      ds
    else
      -.ds
;;



let drawLine scr color line =
  let x1, y1, x2, y2 = line in
  let scrx1 = Util.x2screen x1 !Util.logscreenx 
  and scry1 =(Util.y2screen y1 !Util.logscreeny)
  and scrx2 = Util.x2screen x2 !Util.logscreenx 
  and scry2 =(Util.y2screen y2 !Util.logscreeny) in
    Drawing.drawLine scr color scrx1 scrx2 scry1 scry2;
;;


let makeLine x0 y0 x1 y1 = 
  ( x0, y0, x1, y1 )
;;

let moveLine line xoff yoff =
  let x0, y0, x1, y1 = line in
    ( x0 +. xoff, y0 +. yoff, x1 +. xoff, y0 +. yoff )
;;

let lineIsVert line =
  let x0, _, x1, _ = line in
    x0 = x1
;;

let lineIsHorz line =
  let _, y0, _, y1 = line in
    y0 = y1
;;
  

let line2str line =
  let x1, y1, x2, y2 = line in
  Printf.sprintf "(%f,%f)(%f,%f)" x1 y1 x2 y2
;;

(* Not sure if this is the best way, but... *)
let str2Line str =
  Scanf.sscanf str "(%f,%f)(%f,%f)" makeLine
;;


(* XXX: Grr, this is stupid and reads each file however many times.
   Caching would be nice.
*)
let file2Lines filename =
  let fl = open_in ("config/" ^ filename)
  and lines = ref [] in
    try
      while true do
	lines := (str2Line (input_line fl)) :: !lines;
      done;
      !lines;
    with
	End_of_file ->
	  close_in fl;
(*	  Printf.printf "%d lines read\n" (List.length !lines); *)
	  !lines
;;
