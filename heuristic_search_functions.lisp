  (setf (get 's 'children) '(a b))
  (setf (get 'a 'children) '(s b f))
  (setf (get 'b 'children) '(s a c))
  (setf (get 'c 'children) '(b f))
  (setf (get 'f 'children) '(a c))
  (setf (get 's 'distance) 2)
  (setf (get 'a 'distance) 1)
  (setf (get 'b 'distance) 2)
  (setf (get 'c 'distance) 1)
  (setf (get 'f 'distance) 0)
                                 ;takes path and creates new paths with kids
(defun expand (path)
  (remove-if #'(lambda (path) (member (car path) (cdr path))) ;kill cycles
               (mapcar #'(lambda (child) (cons child path))
                         (get (car path) 'children))))

                                 ;takes path and creates new paths with kids on
(defun another-expand (path)
  (do (new (l2 (get (car path) 'children) (cdr l2)))
      ((null l2) (reverse new))
      (cond ((not (member (car l2) path))
             (setq new (cons (cons (car l2) path) new))))))

(defun depth (start finish)              ;depth first search for path
  (depth1 (list (list start)) finish))   ;from start to finish in net

(defun depth1 (queue finish)             ;helper fcn for depth first
  (cond ((null queue) nil)
        ((equal finish (caar queue)) (print queue)
                                     (reverse (car queue)))
        (t (print queue)
           (depth1 (append (expand (car queue)) (cdr queue)) finish))))

(defun hill (start finish)               ;improved depth first search for path
  (hill1 (list (list start)) finish))    ;from start to finish in net

(defun hill1 (queue finish)              ;helper fcn for hill
  (cond ((null queue) nil)
        ((equal finish (caar queue)) (print queue)
                                     (reverse (car queue)))
        (t (print queue)
           (hill1 (append                        ;call to sort is the change
                     (sort (expand (car queue)) 'closerp)
                     (cdr queue))
                  finish))))

(defun best (start finish)               ;similar to hill climbing, but
  (best1 (list (list start)) finish))    ;entire queue will be sorted

(defun best1 (queue finish)              ;helper fcn for best
  (cond ((null queue) nil)
        ((equal finish (caar queue)) (print queue)
                                     (reverse (car queue)))
        (t (print queue)
           (best1 (sort                          ;order in which queue is
                     (append                     ;expanded and sorted
                        (expand (car queue)) (cdr queue))
                     'closerp)
                   finish))))

(defun breadth (start finish)            ;breadth first search for path
  (breadth1 (list (list start)) finish)) ;from start to finish in net

(defun breadth1 (queue finish)           ;helper fcn for breadth
  (cond ((null queue) nil)
        ((equal finish (caar queue)) (print queue)
                                     (reverse (car queue)))
        (t (print queue)
           (breadth1 (append (cdr queue) (expand (car queue))) finish))))

(defun beam (start finish width)            ;improved breadth first search for
  (beam1 (list (list start)) finish width)) ;path from start to finish in net

(defun beam1 (queue finish width)           ;helper fcn for beam
  (cond ((null queue) nil)
        ((equal finish (caar queue)) (print queue)
                                     (reverse (car queue)))
        (t (print queue)
           (beam1 (sort                          ;check smaller than width?
                     (apply 'nconc (mapcar 'expand
                                      (if (< (length queue) width)
                                            queue (first-n queue width))))
                     'closerp)
                   finish
                   width))))

(defun first-n (l n)                        ;get the first n list elements
  (cond ((zerop n) nil)
        (t (cons (car l) (first-n (cdr l) (1- n))))))

(defun insertion-sort (s predicate)         ;insertion sort for list
  (cond ((null s) nil)
        (t (splice-in (car s) (insertion-sort (cdr s) predicate)
            predicate))))

(defun splice-in (element s predicate)               ;insert in order
  (cond ((null s) (list element))                    ;end of s?
        ((funcall predicate element (car s))         ;add here?
            (cons element s))
        (t (cons (car s)                             ;try further down
           (splice-in element (cdr s) predicate)))))

(defun closerp (x y)
  (< (get (car x) 'distance) (get (car y) 'distance)))

(defun branch (start finish)             ;similar to best, but queue sorted
  (branch1 (list (list start)) finish))  ;based on path length

(defun branch1 (queue finish)            ;helper fcn for branch
  (cond ((null queue) nil)
        ((equal finish (caar queue)) (print queue)
                                     (reverse (car queue)))
        (t (print queue)
           (branch1 (sort                ;new predicate used
                      (append
                         (expand (car queue)) (cdr queue))
                      'shorterp)
                    finish))))

(defun shorterp (p1 p2)                  ;check path lengths
  (< (length p1) (length p2)))

(defun under (start finish)             ;similar branch, but queue sorted
  (under1 (list (list start)) finish))  ;based on path length and nearness

(defun under1 (queue finish)            ;helper fcn for under
  (cond ((null queue) nil)
        ((equal finish (caar queue)) (print queue)
                                     (reverse (car queue)))
        (t (print queue)
           (under1 (sort                ;new predicate used
                      (append
                         (expand (car queue)) (cdr queue))
                      'betterp)
                    finish))))

(defun betterp (p1 p2)                  ;check path length and nearness to goal
  (if (< (+ (length p1) (get (car p1) 'distance))
         (+ (length p2) (get (car p2) 'distance))) t nil))

(defun dynamic (start finish)             ;similar to branch, but redundant
  (dynamic1 (list (list start)) finish))  ;pathes are removed from queue

(defun dynamic1 (queue finish)            ;helper fcn for dynamic
  (cond ((null queue) nil)
        ((equal finish (caar queue)) (print queue)
                                     (reverse (car queue)))
        (t (print queue)
           (dynamic1 (remove-dups
                       (sort              ;use same predicate as branch
                         (append
                           (expand (car queue)) (cdr queue))
                         'shorterp))
                      finish))))

(defun remove-dups (queue)                           ;remove duplicate paths
  (do* ((path (car queue) (cadr (member path queue)))
        (remd (cdr queue) (cdr  (member path queue))))
       ((null remd) queue)
                                            ;drop any path reaching same
       (dolist (path2 remd)                 ;node as first path
          (if (equal (car path2) (car path))
                (setq queue (remove path2 queue))))))

(defun a* (start finish)             ;similar to under, but redundant
  (a*1 (list (list start)) finish))  ;pathes are removed from queue

(defun a*1 (queue finish)            ;helper fcn for a*
  (cond ((null queue) nil)
        ((equal finish (caar queue)) (print queue)
                                     (reverse (car queue)))
        (t (print queue)
           (a*1 (remove-dups
                  (sort              ;use same predicate as under
                    (append
                      (expand (car queue)) (cdr queue))
                    'betterp))
                 finish))))
