; (change-directory "...")
; (compile-file "werthman_assignment1_test.lisp" :load t)

(compile-file "werthman_assignment1.lisp" :load t)

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

; Test case for create a global variable LOC
; that stores Robbies current location
; and set it to pantry
(print "TESTING FOR SET GLOBAL VARIABLE LOC")
(print "Test case 1 for LOC set to Pantry")
(setq LOC 'PANTRY)
(if (equal LOC 'PANTRY)
  (print (format nil "PASS: LOC set to ~a." LOC))
  (print (format nil "FAIL: LOC set to ~a." LOC))
)
(print "")

; Test case for HOW-MANY-CHOICES
; If Robbie is in the PANTRY the function should return 2.
(print "TESTING FOR HOW-MANY-CHOICES")
(print "Test case 1 for HOW-MANY-CHOICES")
(if (equal (HOW-MANY-CHOICES) 2)
  (print (format nil "PASS: HOW-MANY-CHOICES returns ~a." (HOW-MANY-CHOICES)))
  (print (format nil "FAIL: HOW-MANY-CHOICES returns ~a." (HOW-MANY-CHOICES)))
)
(print "")

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