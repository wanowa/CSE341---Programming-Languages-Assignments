;;; Find area
(defun area (width length)
    (return-from area (* width length))
)

(write (area 10 5))


;;;function that takes an item and builds a list of five of them
(defun build(arg)
    (setq myList (cons arg (cons arg (cons arg (cons arg (cons arg ()))))))
    (return-from build myList)
)
(setq var 'Hello)
(write (build var))



;;;predicate that returns the absolute value of a number, using COND
(defun absVal (var)
    
    
    (cond ((< var 0) (setq var (* -1 var))))
    (return-from absVal var)
)

(write (absVal -9))



;;;Checking whether an element is in a list
(defun inList(e l)
    (cond ((null l) nil)
        ((equal (car l) e) t)
        (t (inList e (cdr l)))
    )
)

(write (inList 3 '(1 2 3 4 5)))

;;;Find List Length
(defun listLength(l)
    (setq len 0)
    (loop
        (if (null l) (return len))
        (setq len (+ len 1))
        (setq l (cdr l))
    )
    (write len)
)

(listLength '(1 2 3 4 5 'A 6 'a 7))


;;;Find List Length with Recursion (without loop)
(defun my-length (l)
  (cond ((null l) 0)
        (t (1+ (my-length (cdr l))))))

(write (my-length '(1 2 3 4 5 'A 6 'a 7)))



;;;Listedeki son cons cell'ini çıkarma
(defun my-butlast (l)
   (cond ((null l) nil)
         ((atom (cdr l)) nil)
         ((null (cdr l)) nil)
         (t (cons (car l) (my-butlast (cdr l))))))

(write (my-butlast '(1 (2 3) 4 (5 6 7))))



;;;Listenin son cons cell'ini bulma
(defun my-last(l)
    (cond ((null l)(nil))
        ((null (cdr l)) (return-from my-last l))    ;if the last cons is a normal cons
        ((atom (cdr l)) l)                          ;if the last cons is a dotted pair
        (t (my-last (cdr l)))
    )
)

(write (my-last (cons 3 5)))



;;;Reverse List
(defun my-reverse (l)
  (my-reverse-helper l nil))

(defun my-reverse-helper (l result-so-far)
  (cond ((null l) result-so-far)
        (t (my-reverse-helper (cdr l) (cons (car l) result-so-far)))))
        
(write (my-reverse '(1 2 3 4 5)))



;;;Remove an element from a list
(defun my-remove (l x)
    (cond
        ((null l) nil)
        ((equal (car l) x) (my-remove (cdr l) x))
        (t (cons (car l) (my-remove (cdr l) x)))
    )
)

(write (my-remove '(1 2 3 4 5) 3))


;;;Remove duplicate in a list
(defun duplicate-remove(l)
    (cond
        ((null l) nil)
        ((member (car l) (cdr l)) (duplicate-remove (cdr l)))
        (t (cons (car l) (duplicate-remove (cdr l))))
    )
)

(write (duplicate-remove '(1 2 2 2 2 3 4 4 4 5 5)))


;;;Append two lists
(defun my-append (l1 l2)
    (my-append-helper l1 l2 nil)
)

(defun my-append-helper (l1 l2 result)
    (cond ((and (null l1) (null l2)) result)
        ((null l1) (cons (car l2) (my-append-helper l1 (cdr l2) result)))
        (t (cons (car l1) (my-append-helper (cdr l1) l2 result)))
    )
)

(write (my-append '(1 2 3) '(4 5 6)))



;;;Doubles each element in the list
(defun double-list (l)
    (cond 
        ((null l) nil)
        (t (cons (car l) (cons (car l) (double-list (cdr l)))))
    )
)

(write (double-list '(1 2 3 4 5)))



;;;Adds up all of the numbers in a list
(defun my-sum (l)
    (cond
        ((null l) 0)
        ((not (numberp (car l))) (my-sum (cdr l)))
        (t (+ (car l) (my-sum (cdr l))))
    )

)

(write (my-sum '(1 2 3 4 5)))



;;;Reverse the List again
(defun reverse2(l)
    (reverse-helper l nil)
)
(defun reverse-helper(l result)
    (cond
        ((null l) result)
        (t (reverse-helper (cdr l) (cons (car l) result)))
    )
)

(write (reverse2 '(1 2 3 4 5)))



;;;Shallow recursion count item
(defun my-count (x l)
  (cond ((null l) 0)
        ((equal x (car l)) (1+ (my-count x (cdr l))))
        (t (my-count x (cdr l)))))

(my-count 'x '((a x) (b x) (c x) d)) ; returns 0



;;;Deep recursion count item
(defun deep-count (x l)
  (cond ((null l) 0)
        ((equal x l) 1)  ; our "list" is actually the element - this replaces the CAR case from before
        ((atom l) 0) ; our "list" is an atom that doesn't match - can't recurse anymore
        ;;Recursive case has two recursions that are then added together
        (t (+ (deep-count x (car l)) (deep-count x (cdr l))))))

(deep-count 'x '(a x b x c x d)) ;returns 3



;;;My Shallow Recursion count item
(defun sayac(l x)
    (cond
        ((null l) 0)
        ((equal (car l) x) (+ 1 (sayac (cdr l) x)))
        (t (+ 0 (sayac (cdr l) x)))
    )
)

(write (sayac '(1 2 2 3 3 3 3 3) 3))



;;;My Deep Recursion count item
(defun derin-sayac(l x)
    (cond
        ((null l) 0)
        ((equal l x) 1)
        ((atom l) 0)
        (t (+ (derin-sayac (car l) x) (derin-sayac (cdr l) x) ))
    )
)

(write (derin-sayac '(a x b x c x d) 'x))



;;;Substitutions in a list
(defun my-subst(new old myList)
    (cond
        ((null myList) nil)
        ((equal old myList) new)
        ((atom myList) myList)
        (t (cons (my-subst new old (car myList)) (my-subst new old (cdr myList))))
    )
)

(write (my-subst 'yetea 'b '(a  b (d b) c b d a b)))




;;;;;;;;;;;;;;;;;;;;;;                                                  
(defun carpiiki(x)
    (* x x)
)

;;;
(defun my-mapcar (func lst)
   (cond ((null lst) nil)
         (t (cons (funcall func (car lst)) 
                  (my-mapcar func (cdr lst))))))


(my-mapcar(#'1+ '(1 2 3 4 5)))
;;;;;;;;;;;;;;;;;;;;;;                           


;;nth element of a List
(defun nthVar (l n)
    (cond
        ((null l) nil)
        ((equal n 0) (car l)) 
        (t (nthVar (cdr l) (decf n 1)))
    )
)

(write (nthVar '(1 5 7 2 6 7 4) 2))