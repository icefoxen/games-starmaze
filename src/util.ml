(* util.ml
   Basic utility junk and global vars.
   
   Simon Heath
*)

open Sdlvideo;;

let screenx = ref 800;;
let screeny = ref 600;;
let logscreenx = ref 0.;;
let logscreeny = ref 0.;;
let logscreenw = ref 0.;;
let logscreenh = ref 0.;;



let timecompress = ref 1;;
let gravity = ref (-0.10);;
let gravlimit = ref (-300.);;


let right = 1.
and left = -1.;;


let print_bool = function
    true -> print_string "true"
  | false -> print_string "false"
;;

let error x y = 
  Printf.eprintf x y;
  exit 1;
;;


let pi = acos (-1.0);;
let d2r x = x *. (pi /. 180.);;
let r2d x = x *. (180. /. pi);;
let absf x =
  if x < 0. then
    -.x
  else
    x
;;

let absmod x y = 
  let n = x mod y in
    abs n
;;

let fabs x =
  if x < 0. then
    -. x
  else
    x
;;


let incf x =
  x := !x +. 1.;;

let decf x = 
  x := !x -. 1.;;

let x2screen gx scrx =
  (int_of_float (gx -. scrx +. (float_of_int (!screenx / 2))))
;;

let y2screen gy scry =
  (int_of_float (-1. *. gy +. scry +. (float_of_int (!screeny / 2))))
;;


let removeNth lst n =
  let rec loop n lst = 
    if n = 0 then
      List.tl lst
    else 
      (List.hd lst) :: (loop (n - 1) (List.tl lst))
  in
    if List.length lst > n then
      raise (Failure "removeNth: list too long")
    else
      loop n lst
;;


let square x =
  x *. x
;;

(* Return true if a is equal to b within the given delta *)
let within a b delta =
  absf (a -. b) > delta
;;



(* Why can't I just chop the first or last x characters from a string,
   easily?
*)
let chop_left s i =
  let ns = String.create ((String.length s) - i) in
    for x = i to ((String.length s) - 1) do
      ns.[x - i] <- s.[x]
    done;
    ns
;;

let chop_right s i =
  String.sub s 0 ((String.length s) - i)
;;

(* Trims whitespace from the beginning and end of a string *)
let trim s =
  let stptr = ref 0 
  and endptr = ref ((String.length s) - 1)
  in
    while s.[!stptr] = ' ' || s.[!stptr] = '\t' || s.[!stptr] = '\n' do
      incr stptr;
    done;
    while s.[!endptr] = ' ' || s.[!endptr] = '\t' || s.[!endptr] = '\n' do
      decr endptr;
    done;
    let strlen = !endptr - !stptr + 1 in
      String.sub s !stptr strlen
;;

(* Returns true if the two given rects overlap on the screen *)
let rectsOverlap r1 r2 =
  let left1 = r1.r_x
  and left2 = r2.r_x
  and top1 = r1.r_y
  and top2 = r2.r_y
  and right1 = r1.r_x + r1.r_w
  and right2 = r2.r_x + r2.r_w
  and bottom1 = r1.r_y + r1.r_h
  and bottom2 = r2.r_y + r2.r_h in
    if bottom1 < top2 then
      false
    else if top1 > bottom2 then
      false
    else if right1 < left2 then
      false
    else if left1 > right2 then
      false
    else
      true
;;
