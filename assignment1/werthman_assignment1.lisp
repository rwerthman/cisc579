

; Here is the map Robbie will be using
(DEFVAR ROOMS)
(SETQ ROOMS 
  '((LIVING-ROOM        (NORTH FRONT-STAIRS)
                        (SOUTH DINING-ROOM)
                        (EAST KITCHEN))
    (UPSTAIRS-BEDROOM   (WEST LIBRARY)
                        (SOUTH FRONT-STAIRS))
    (DINING-ROOM        (NORTH LIVING-ROOM)
                        (EAST PANTRY)
                        (WEST DOWNSTAIRS-BEDROOM))
    (KITCHEN            (WEST LIVING-ROOM)
                        (SOUTH PANTRY))
    (PANTRY             (NORTH KITCHEN)
                        (WEST DINING-ROOM))
    (DOWNSTAIRS-BEDROOM (NORTH BACK-STAIRS)
                        (EAST DINING-ROOM))
    (BACK-STAIRS        (SOUTH DOWNSTAIRS-BEDROOM)
                        (NORTH LIBRARY))
    (FRONT-STAIRS       (NORTH UPSTAIRS-BEDROOM)
                        (SOUTH LIVING-ROOM))
    (LIBRARY            (EAST UPSTAIRS-BEDROOM)
                        (SOUTH BACK-STAIRS)))
)

;A function CHOICES which takes the name of the room and returns 
;the table of permissible choices for Robbie's next destination. 
;For example (CHOICES 'PANTRY) returns the list ((NORTH KITCHEN) 
;(WEST DINING-ROOM)).

(defun CHOICES (x)
  (
    ; look for a sublist in ROOMS containing x
    let ((Y (assoc x ROOMS)))
    ; return the rest of the sublist containing x
    ; while not return x
      (CDR Y) 
  )
)

;The function LOOK that takes two inputs, a direction and a room, and tells 
; where he will end up if he moved in that direction from that room. For example if 
;Robbie were to (LOOK 'NORTH 'PANTRY) he would get (KITCHEN), (LOOK 'WEST 'PANTRY) 
; gives him (DINING-ROOM), (LOOK 'SOUTH 'PANTRY)gives NIL. (Hint: Use CHOICES somewhere.)

(defun LOOK (direction roomofhouse)
  (
    ; Get the sublist of the room of the house robbie wants to go
    ; Get the room in the direction he wants to go
    let* ((x (CHOICES roomofhouse)) (y (assoc direction x)))
    (CDR y)

  )
)

; An expression which sets a global variable LOC to hold Robbie's current 
; position in the PANTRY
(DEFVAR LOC)
(setq LOC 'PANTRY)

; A function HOW-MANY-CHOICES that tells how many choices Robbie has 
; for where he may move given the current value of the variable LOC. 
; If Robbie is in the PANTRY the function should return 2.
(defun HOW-MANY-CHOICES ()
  (
    let ((x (CHOICES LOC)))
    (length x)
  )
)

; A predicate function UPSTAIRSP that returns T if the input is an 
; upstairs locations The UPSTAIRS-BEDROOM and the LIBRARY are the only rooms 
; which qualify
(defun UPSTAIRSP (location)
  ; Check if the give location is one of the upstairs rooms
  (cond ((equal location 'UPSTAIRS-BEDROOM) t)
        ((equal location 'LIBRARY) t)
        (nil)
  )
)

;A predicate function ONSTAIRSP which returns T if it’s input is either 
;FRONT-STAIRS or BACK-STAIRS
(defun ONSTAIRSP (location)
  ; Check if the give location is on the stairs
  (cond ((equal location 'BACK-STAIRS) t)
        ((equal location 'FRONT-STAIRS) t)
        (nil)
  )
)

;A function WHERE that requires no inputs and tells Robbie where he is. 
;If he is in the LIBRARY where should return(ROBBIE IS UPSTAIRS IN THE LIBRARY),
; if he is in the KITCHEN it should return (ROBBIE IS DOWNSTAIRS IN THE KITCHEN), 
;and if he is on the front stairs it should return (ROBBIE IS ON THE 
; FRONT-STAIRS). (Hint: Use your predicate functions.)
(defun WHERE ()
  ; Check if Robbie is upstairs, downstairs, or on the stairs and return
  ; related string
  (cond ((equal (ONSTAIRSP LOC) t) (append '(ROBBIE IS ON THE) (list LOC)))
        ((equal (UPSTAIRSP LOC) t) (append '(ROBBIE IS UPSTAIRS IN THE) (list LOC)))
        ((append '(ROBBIE IS DOWNSTAIRS IN THE) (list LOC)))

  )
)

;A function MOVE that takes one input, a direction, and moves Robbie in that 
;direction. MOVE should make use of your LOOK function. If he can't move in 
;the requested direction, an appropriate message should be returned. 
;For example, if Robbie is in the PANTRY (MOVE 'SOUTH) should return 
;(OUCH! ROBBIE HIT A WALL), while (MOVE 'NORTH) should return (ROBBIE IS 
;DOWNSTAIRS IN THE KITCHEN.
(defun MOVE (direction)
  ; If robbie can't move to a place then he hits wall
  (cond ((equal (LOOK direction LOC) nil) '(OUCH! ROBBIE HIT A WALL))
        (
          ; Otherwise, Robbie moves to the new location
          (setq LOC (car (LOOK direction LOC)))
          ; Return the description of the new location
          (WHERE)
        )

  )
)

;Adapt one of the search functions presented in class to help Robbie 
;find a path from PANTRY to the KITCHEN thatpasses through the LIBRARY
; I'm going to adapt branch and bound to this function
(defun ROBBIESEARCH (source destination waypoint)
  (branch source destination waypoint)
)

(defun branch (start finish waypoint)             ;similar to best, but queue sorted
  (branch1 (list (list start)) finish waypoint))  ;based on path length

(defun branch1 (queue finish waypoint)            ;helper fcn for branch
  (cond ((null queue) nil)
        ; We can only finish when our destination and waypoint are in the path
        ((and (equal finish (caar queue)) (member waypoint (car queue))) (print queue)
                                     (reverse (car queue)))
        (t ;(print queue)
           (branch1 (sort                ;new predicate used
                      (append
                         (expand (car queue)) (cdr queue))
                      'shorterp)
                    finish waypoint
                    ))))

(defun shorterp (p1 p2)                  ;check path lengths
  (< (length p1) (length p2)))

(defun expand (path)
  (remove-if #'(lambda (path) (member (car path) (cdr path))) ;kill cycles
               (mapcar #'(lambda (child) (cons child path))
                         ; Get the children of each room in the ROOMS variable
                         (apply 'append (mapcar #'(lambda (children) (car children) (cdr children)) (CHOICES (car path))))
                )
  )
)
