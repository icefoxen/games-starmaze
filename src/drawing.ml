(* drawing.ml
   Line, circle and polygon drawing functions.
   Vector graphics, basically.

   ALL of these take screenworld coordinates!

   TODO: 
     Draw with alpha
     Draw filled circles?
     Draw polygons/triangles?
     Draw filled polygons/triangles?


*)

open Sdlvideo
open Util


let drawFilledRect surf x y w h col =
  let r = {r_x = x; r_y = y; r_w = w; r_h = h} in
   fill_rect ~rect: r surf (map_RGB surf col)
;;


(* Bresenham line drawing

Takes screenworld coords and a color.

function line(x0, x1, y0, y1)
     boolean steep := abs(y1 - y0) > abs(x1 - x0)
     if steep then
         swap(x0, y0)
         swap(x1, y1)
     if x0 > x1 then
         swap(x0, x1)
         swap(y0, y1)
     int deltax := x1 - x0
     int deltay := abs(y1 - y0)
     int error := 0
     int ystep
     int y := y0
     if y0 < y1 then ystep := 1 else ystep := -1
     for x from x0 to x1
         if steep then plot(y,x) else plot(x,y)
         error := error + deltay
         if 2*error >= deltax
             y := y + ystep
             error := error - deltax
*)

let swap a b =
  let c = !b in
    b := !a;
    a := c
;;

(* We check per-pixel if we're actually on the screen or not.
   If not, we don't draw the pixel.
   XXX: A more efficient way would be to check the endpoints and figure out
   how far off the screen they are, then start drawing in the middle.
   However, that's more annoying to write.
   We'll have to do it sooner or later though, I suspect.

   Okay, here's how it should work:
   1) If both the endpoints are to the left, to the right, above,
   or below the screen-rectangle.  If so, don't draw.
   2) If both the endpoints are in the screen-rectangle, draw it.
   3) If one endpoint is in the screen-rectangle, draw from that endpoint
   until we are no longer in the screen, and stop.  This is sorta a subset
   of the above...
   4) Else, we have two endpoints off the screen in different quadrants.
   We get the equation of the line, and find... The y value at each end of
   the rectangle, and the x value at the top and bottom of the rectangle.
   If these values are within the rectangle, we draw that segment.
*)
let drawLine scr color x0 x1 y0 y1 =
  if must_lock scr then
    lock scr;

  let steep = (abs (y1 - y0)) > (abs (x1 - x0)) in
  let x0 = ref x0
  and x1 = ref x1
  and y0 = ref y0
  and y1 = ref y1 in
    if steep then (
	swap x0 y0;
	swap x1 y1;
      );
    if !x0 > !x1 then (
	swap x0 x1;
	swap y0 y1;
      );
    let deltax = !x1 - !x0
    and deltay = abs (!y1 - !y0)
    and error = ref 0
    and ystep = (if !y0 < !y1 then 1 else -1)
    and y = ref !y0 in
      for x = !x0 to !x1 do

	(* We now have lines that are 2 pixels wide, with a bit of dark
	   shadow under the horizontal lines to give 'em definition.
	   Looks okay, really.
	*)
	if steep then (
	    if (!y >= 0) && (!y < !screenx) &&
	      (x >= 0) && (x < !screeny) then (
		  put_pixel_color scr ~x: !y ~y: x color;
		  put_pixel_color scr ~x: (!y+1) ~y: x color;
		)
	  ) else (
	    if (x >= 0) && (x < !screenx) &&
	      (!y >= 0) && (!y < !screeny) then (
		  put_pixel_color scr ~x: x ~y: !y color;
		  put_pixel_color scr ~x: x ~y: (!y+1) color;
		  put_pixel_color scr ~x: x ~y: (!y+2) (0,0,0)
		)
	  );

	error := !error + deltay;

	if (2 * !error) >= deltax then (
	    y := !y + ystep;
	    error := !error - deltax
	  );
      done;
      
      if must_lock scr then
	unlock scr;
;;



(* Circle drawing, on the midpoint algorithm.
private final void circlePoints(int cx, int cy, int x, int y, int pix)
    {
        int act = Color.red.getRGB();
        
        if (x == 0) {
            raster.setPixel(act, cx, cy + y);
            raster.setPixel(pix, cx, cy - y);
            raster.setPixel(pix, cx + y, cy);
            raster.setPixel(pix, cx - y, cy);
        } else 
        if (x == y) {
            raster.setPixel(act, cx + x, cy + y);
            raster.setPixel(pix, cx - x, cy + y);
            raster.setPixel(pix, cx + x, cy - y);
            raster.setPixel(pix, cx - x, cy - y);
        } else 
        if (x < y) {
            raster.setPixel(act, cx + x, cy + y);
            raster.setPixel(pix, cx - x, cy + y);
            raster.setPixel(pix, cx + x, cy - y);
            raster.setPixel(pix, cx - x, cy - y);
            raster.setPixel(pix, cx + y, cy + x);
            raster.setPixel(pix, cx - y, cy + x);
            raster.setPixel(pix, cx + y, cy - x);
            raster.setPixel(pix, cx - y, cy - x);
        }
    }

    public void circleMidpoint(int xCenter, int yCenter, int radius, Color c)
    {
        int pix = c.getRGB();
        int x = 0;
        int y = radius;
        int p = (5 - radius*4)/4;

        circlePoints(xCenter, yCenter, x, y, pix);
        while (x < y) {
            x++;
            if (p < 0) {
                p += 2*x+1;
            } else {
                y--;
                p += 2*(x-y)+1;
            }
            circlePoints(xCenter, yCenter, x, y, pix);
        }
    }

*)

let circlePoints scr color cx cy x y =
  if x == 0 then (
    put_pixel_color scr ~x: cx ~y: (cy + y) color;
    put_pixel_color scr ~x: cx ~y: (cy - y) color;
    put_pixel_color scr ~x: (cx + y) ~y: cy color;
    put_pixel_color scr ~x: (cx - y) ~y: cy color;
  ) else if x == y then (
    put_pixel_color scr ~x: (cx + x) ~y: (cy + y) color;
    put_pixel_color scr ~x: (cx - x) ~y: (cy + y) color;
    put_pixel_color scr ~x: (cx + x) ~y: (cy - y) color;
    put_pixel_color scr ~x: (cx - x) ~y: (cy - y) color;
  ) else if x < y then (
    put_pixel_color scr ~x: (cx + x) ~y: (cy + y) color;
    put_pixel_color scr ~x: (cx - x) ~y: (cy + y) color;
    put_pixel_color scr ~x: (cx + x) ~y: (cy - y) color;
    put_pixel_color scr ~x: (cx - x) ~y: (cy - y) color;
    put_pixel_color scr ~x: (cx + y) ~y: (cy + x) color;
    put_pixel_color scr ~x: (cx - y) ~y: (cy + x) color;
    put_pixel_color scr ~x: (cx + y) ~y: (cy - x) color;
    put_pixel_color scr ~x: (cx - y) ~y: (cy - x) color;
  )
;;


(* Bounds-checking!  Imperfect, but functional.  Beware big circles.
   It won't draw if the /center/ is off the screen...
   ...no, not functional.  WTF?
*)
let drawCircle scr color xCenter yCenter radius =
  if (xCenter > 0) && (xCenter < !screenx) &&
    (yCenter > 0) && ((yCenter + radius) < !screeny) then (
(*      Printf.printf "Circle: %d,%d\n" xCenter yCenter; *)

      let x = ref 0 in
      let y = ref radius in
      let p = ref ((5 - radius * 4) / 4) in  (* 5?  Why 5? *)
	circlePoints scr color xCenter yCenter !x !y;
	while !x < !y do
	  incr x;
	  
	  if !p < 0 then (
	    p := !p + (2 * !x + 1)
	  ) else (
	    decr y;
	    p := !p + (2 * (!x - !y) + 1);
	  );

	  circlePoints scr color xCenter yCenter !x !y;
	done;
    )
      
;;



(* Inefficient, but I guess it works. *)
let rec drawFilledCircle scr color xCenter yCenter radius =
  if radius = 0 then
    ()
  else (
      drawCircle scr color xCenter yCenter radius;
      drawFilledCircle scr color xCenter yCenter (radius-1);
    )
;;


(* Draws an outline of a rectangle *)
let drawRect surf col x y w h =
  drawLine surf col x (x + w) y y;
  drawLine surf col (x + w) (x + w) y (y + h);
  drawLine surf col x x y (y + h);
  drawLine surf col x (x + w) (y + h) (y + h);
;;



(* This fills an area with a color, overwriting everything
   until it hits boundaryColor.  
   If it never hits boundaryColor, it will just go off the
   edge of the screen and segfault something.

   ...hmm.  It's not tail-recursive, either.
   Might be able to change that and make fewer false checks
   with a bit of mutual recursion, but it'd be weird.

   XXX: Broken.  Never use.
*)
let rec floodFill surf col boundaryColor x y =
  Printf.printf "Flooding %d, %d\n" x y;
  let pix = get_pixel_color surf ~x: x ~y: y in
  if (pix = boundaryColor) or (pix = col) then
    print_endline "Terminated!"
  else (
      put_pixel_color surf ~x: x ~y: y col;
      flip surf;
      floodFill surf col boundaryColor (x+1) y;
      floodFill surf col boundaryColor (x-1) y;
(*      floodFill surf col boundaryColor x (y+1); *)
(*      floodFill surf col boundaryColor x (y-1); *)

    )
;;


let drawPoint scr col x y =
  put_pixel_color scr ~x: x ~y: y col
;;
