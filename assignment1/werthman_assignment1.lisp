(defun batch (fn1 fn2)    
(do* ((old (open fn1 :direction :input))            
    (new (open fn2 :direction :output))           
    (ex (read old nil) (read old nil))           
    )                                             
    ((null ex) (close old)                     
             (princ "end of program" new)                    
             (terpri new)                    
             (close new)         
    )         
    (print ex new)         
    (print (eval ex) new)         
    (terpri new)     
    )
)

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

; Test case for the function CHOICES
; If name of room is PANTRY then return ((NORTH KITCHEN)(WEST DINING-ROOM))
(print "TESTING FOR CHOICES FUNCTION")
(print "Test case 1 for CHOICES (CHOICES 'PANTRY)")
(if (equal (CHOICES 'PANTRY) '((NORTH KITCHEN)(WEST DINING-ROOM)))
  (print (format nil "PASS: CHOICES returned ~a." (CHOICES 'PANTRY)))
  (print (format nil "FAIL: CHOICES returned ~a." (CHOICES 'PANTRY)))
)
(print "")

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

; Test cases for the function LOOK
; LOOK 'NORTH 'PANTRY) he would get (KITCHEN), (LOOK 'WEST 'PANTRY) 
; gives him (DINING-ROOM), (LOOK 'SOUTH 'PANTRY)gives NIL
(print "TESTING FOR LOOK FUNCTION")
(print "Test case 1 for LOOK LOOK 'NORTH 'PANTRY")
(if (equal (LOOK 'NORTH 'PANTRY) '(KITCHEN))
  (print (format nil "PASS: LOOK returned ~a." (LOOK 'NORTH 'PANTRY)))
  (print (format nil "FAIL: LOOK returned ~a." (LOOK 'NORTH 'PANTRY)))
)
(print "")

(print "Test case 2 for LOOK LOOK 'WEST 'PANTRY")
(if (equal (LOOK 'WEST 'PANTRY) '(DINING-ROOM))
  (print (format nil "PASS: LOOK returned ~a." (LOOK 'WEST 'PANTRY)))
  (print (format nil "FAIL: LOOK returned ~a." (LOOK 'WEST 'PANTRY)))
)
(print "")

(print "Test case 3 for LOOK LOOK 'SOUTH 'PANTRY")
(if (equal (LOOK 'SOUTH 'PANTRY) nil)
  (print (format nil "PASS: LOOK returned ~a." (LOOK 'SOUTH 'PANTRY)))
  (print (format nil "FAIL: LOOK returned ~a." (LOOK 'SOUTH 'PANTRY)))
)
(print "")

; An expression which sets a global variable LOC to hold Robbie's current 
; position in the PANTRY
(DEFVAR LOC)
(setq LOC 'PANTRY)

(print "TESTING FOR SET GLOBAL VARIABLE LOC")
(print "Test case 1 for LOC set to Pantry")
(if (equal LOC 'PANTRY)
  (print (format nil "PASS: LOC set to ~a." LOC))
  (print (format nil "FAIL: LOC set to ~a." LOC))
)
(print "")

; A function HOW-MANY-CHOICES that tells how many choices Robbie has 
; for where he may move given the current value of the variable LOC. 
; If Robbie is in the PANTRY the function should return 2.
(defun HOW-MANY-CHOICES ()
  (
    let ((x (CHOICES LOC)))
    (length x)
  )
)

; Test case for HOW-MANY-CHOICES
; If Robbie is in the PANTRY the function should return 2.
(print "TESTING FOR HOW-MANY-CHOICES")
(print "Test case 1 for HOW-MANY-CHOICES")
(if (equal (HOW-MANY-CHOICES) 2)
  (print (format nil "PASS: HOW-MANY-CHOICES returns ~a." (HOW-MANY-CHOICES)))
  (print (format nil "FAIL: HOW-MANY-CHOICES returns ~a." (HOW-MANY-CHOICES)))
)
(print "")

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

; Test case for UPSTAIRSP
; The UPSTAIRS-BEDROOM and the LIBRARY will return t
; All others will return nil
(print "TESTING FOR UPSTAIRSP")
(print "Test case 1 for UPSTAIRSP LIBRARY")
(if (equal (UPSTAIRSP 'LIBRARY) t)
  (print (format nil "PASS: UPSTAIRSP returns ~a." (UPSTAIRSP 'LIBRARY)))
  (print (format nil "FAIL: UPSTAIRSP returns ~a." (UPSTAIRSP 'LIBRARY)))
)
(print "")

(print "Test case 2 for UPSTAIRSP UPSTAIRS-BEDROOM")
(if (equal (UPSTAIRSP 'UPSTAIRS-BEDROOM) t)
  (print (format nil "PASS: UPSTAIRSP returns ~a." (UPSTAIRSP 'UPSTAIRS-BEDROOM)))
  (print (format nil "FAIL: UPSTAIRSP returns ~a." (UPSTAIRSP 'UPSTAIRS-BEDROOM)))
)
(print "")

(print "Test case 3 for UPSTAIRSP PANTRY")
(if (equal (UPSTAIRSP 'PANTRY) nil)
  (print (format nil "PASS: UPSTAIRSP returns ~a." (UPSTAIRSP 'PANTRY)))
  (print (format nil "FAIL: UPSTAIRSP returns ~a." (UPSTAIRSP 'PANTRY)))
)
(print "")

;A predicate function ONSTAIRSP which returns T if it’s input is either 
;FRONT-STAIRS or BACK-STAIRS
(defun ONSTAIRSP (location)
  ; Check if the give location is on the stairs
  (cond ((equal location 'BACK-STAIRS) t)
        ((equal location 'FRONT-STAIRS) t)
        (nil)
  )
)

; Test case for ONSTAIRSP
; The FRONT-STAIRS and the BACK-STAIRS will return t
; All others will return nil
(print "TESTING FOR ONSTAIRSP")
(print "Test case 1 for ONSTAIRSP FRONT-STAIRS")
(if (equal (ONSTAIRSP 'FRONT-STAIRS) t)
  (print (format nil "PASS: ONSTAIRSP returns ~a." (ONSTAIRSP 'FRONT-STAIRS)))
  (print (format nil "FAIL: ONSTAIRSP returns ~a." (ONSTAIRSP 'FRONT-STAIRS)))
)
(print "")

(print "Test case 2 for ONSTAIRSP BACK-STAIRS")
(if (equal (ONSTAIRSP 'BACK-STAIRS) t)
  (print (format nil "PASS: ONSTAIRSP returns ~a." (ONSTAIRSP 'BACK-STAIRS)))
  (print (format nil "FAIL: ONSTAIRSP returns ~a." (ONSTAIRSP 'BACK-STAIRS)))
)
(print "")

(print "Test case 3 for ONSTAIRSP PANTRY")
(if (equal (ONSTAIRSP 'PANTRY) nil)
  (print (format nil "PASS: ONSTAIRSP returns ~a." (ONSTAIRSP 'PANTRY)))
  (print (format nil "FAIL: ONSTAIRSP returns ~a." (ONSTAIRSP 'PANTRY)))
)
(print "")

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

; Test case for WHERE
; If he is in the LIBRARY where should return(ROBBIE IS UPSTAIRS IN THE LIBRARY),
; if he is in the KITCHEN it should return (ROBBIE IS DOWNSTAIRS IN THE KITCHEN), 
; and if he is on the front stairs it should return (ROBBIE IS ON THE 
; FRONT-STAIRS)
(print "TESTING FOR WHERE")
(print "Test case 1 for WHERE LIBRARY")
(setq LOC 'LIBRARY)
(if (equal (WHERE) '(ROBBIE IS UPSTAIRS IN THE LIBRARY))
  (print (format nil "PASS: WHERE returns ~a." (WHERE)))
  (print (format nil "FAIL: WHERE returns ~a." (WHERE)))
)
(print "")

(print "Test case 2 for WHERE KITCHEN")
(setq LOC 'KITCHEN)
(if (equal (WHERE) '(ROBBIE IS DOWNSTAIRS IN THE KITCHEN))
  (print (format nil "PASS: WHERE returns ~a." (WHERE)))
  (print (format nil "FAIL: WHERE returns ~a." (WHERE)))
)
(print "")

(print "Test case 3 for WHERE FRONT-STAIRS")
(setq LOC 'FRONT-STAIRS)
(if (equal (WHERE) '(ROBBIE IS ON THE FRONT-STAIRS))
  (print (format nil "PASS: WHERE returns ~a." (WHERE)))
  (print (format nil "FAIL: WHERE returns ~a." (WHERE)))
)
(setq LOC 'PANTRY)
(print "")

;A function MOVE that takes one input, a direction, and moves Robbie in that 
;direction. MOVE should make use of your LOOK function. If he can't move in 
;the requested direction, an appropriate message should be returned. 
;For example, if Robbie is in the PANTRY (MOVE 'SOUTH) should return 
;(OUCH! ROBBIE HIT A WALL), while (MOVE 'NORTH) should return (ROBBIE IS 
;DOWNSTAIRS IN THE KITCHEN.
(defun MOVE (direction)
  (cond ((equal (LOOK direction LOC) nil) '(OUCH! ROBBIE HIT A WALL))
        (
          ; Robbie moves to the new location
          (setq LOC (car (LOOK direction LOC)))
          ; Return the description of the new location
          (WHERE)
        )

  )
)

; Test case for MOVE
;For example, if Robbie is in the PANTRY (MOVE 'SOUTH) should return 
;(OUCH! ROBBIE HIT A WALL), while (MOVE 'NORTH) should return (ROBBIE IS 
;DOWNSTAIRS IN THE KITCHEN.
(print "TESTING FOR MOVE")
(print "Test case 1 for MOVE SOUTH")
(setq LOC 'PANTRY)
(let ((x (MOVE 'SOUTH)))
  (if (equal x '(OUCH! ROBBIE HIT A WALL))
    (print (format nil "PASS: MOVE returns ~a." (MOVE 'SOUTH)))
    (print (format nil "FAIL: MOVE returns ~a." (MOVE 'SOUTH)))
  )
)
(print "")

(print "Test case 2 for MOVE NORTH")
(setq LOC 'PANTRY)
(let ((x (MOVE 'NORTH)))
  (if (equal x '(ROBBIE IS DOWNSTAIRS IN THE KITCHEN))
    (print (format nil "PASS: MOVE returns ~a." x))
    (print (format nil "FAIL: MOVE returns ~a." x))
  )
)
(print "")

 ;(batch "/Users/bobby/Desktop/werthman_assignment1.lisp" "/Users/bobby/Desktop/output.txt")