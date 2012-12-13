(* drawing.ml
   Line, circle and polygon drawing functions.
   Vector graphics, basically.

   ALL of these take screenworld coordinates!

   TODO: 
     Draw with alpha
     Draw circles
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
   We can make it slightly more efficient; if both endpoints are off
   the screen, we don't draw it.  No, that actually won't work.
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

	
	if steep then (
	  if (!y >= 0) && (!y < !screenx) &&
	    (x >= 0) && (x < !screeny) then
	      put_pixel_color scr ~x: !y ~y: x color
	) else (
	  if (x >= 0) && (x < !screenx) &&
	    (!y >= 0) && (!y < !screeny) then
	      put_pixel_color scr ~x: x ~y: !y color;
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





(* Draws an outline of a rectangle *)
let drawRect surf col x y w h =
  drawLine surf col x (x + w) y y;
  drawLine surf col (x + w) (x + w) y (y + h);
  drawLine surf col x x y (y + h);
  drawLine surf col x (x + w) (y + h) (y + h);
;;

