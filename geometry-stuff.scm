(define (makeLine x1 y1 x2 y2)
  (vector x1 y1 x2 y2))

(define (makePoint x y)
  (vector x y))

(define (x1 l)
  (vector-ref l 0))

(define (y1 l)
  (vector-ref l 1))

(define (x2 l)
  (vector-ref l 2))

(define (y2 l)
  (vector-ref l 3))

(define (dx l)
  (- (x2 l) (x1 l)))

(define (dy l)
  (- (y2 l) (y1 l)))

(define (slope l)
  (/ (dy l) (dx l)))

(define (janky-solution l p)
  (let* ((line2 (makeLine (x1 l) (y1 l) (x1 p) (y1 p)))
         (s (slope l))
         (s2 (slope line2)))
    (- s s2)))

(define (point-relative l p)
  (let ((s (slope l))
        (j (janky-solution l p)))
    (if (> s 0)
        (if (> j 0)
            'right
            'left)
        (if (> j 0)
            'left
            'right))))

#| 
Icefox: Just take the dot product of the line's
normal vector (yeah, there are two, pick one of them and stick with
it), and a vector from a point on the line to the point being
tested. If it's 0, it's on the line. If >0, it's on the same side of
the line as the normal is pointing, if <0, it's on the other side.
|#

(define (line->vector l)
  (vector (dx l) (dy l)))

(define (vecthingy l p)
  (line->vector (makeLine (x1 l) (y1 l) (x1 p) (y1 p))))

(define (normal vec)
  (make-vector (y1 vec) (- (x1 vec))))


(define (dot v1 v2)
  (+ (* (x1 v1) (x1 v2)) (* (y1 v1) (y1 v2))))

(define (mjolnir-rocks l p)
  (let ((v1 (vecthingy l p))
        (v2 (normal (line->vector l))))
    (dot v1 v2)))
  
  
 

; (cx-bx)*(bx-ax)+(cy-by)*(by-ay)
; where a is point and b and c are points on line segment.
; dp:{add:
;      {mult:{subt:{:3},{:1}},{subt:{:5},{:3}}},
;      {mult:{subt:{:4},{:2}},{subt:{:6},{:4}}}}
; Type @mpi {dp:4,0,5,1,6,-1} and @mpi {dp:4,0,5,1,6,4}
; That 4 0 and 5 1 are two points on line segment, the last is
;     .. the point.
(define (dp l p)
  (let ((ax (x1 p))
        (bx (x1 l))
        (cx (x2 l))
        (ay (y1 p))
        (by (y1 l))
        (cy (y2 l)))
    (+ (* (- cx bx) (- bx ax)) (* (- cy by) (- by ay)))))

                                     
(define l1 (makeLine 0 0 10 10))
(define p1 (makePoint 5 9))
(define p2 (makePoint 5 1))

(define l2 (makeLine 0 10 10 0))


