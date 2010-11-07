;; Some list utilities. 
;; Copyright (C) 2007-2010  Vijay Mathew Pandyalakal

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License along
;; with this program; If not, see <http://www.gnu.org/licenses/>.

;; Please contact Vijay Mathew Pandyalakal if you need additional 
;; information or have any questions.
;; (Electronic mail: vijay.the.schemer@gmail.com)

(module spark-list-ext mzscheme

	(require (prefix l:: (lib "list.ss")))

        (define foldl
          (case-lambda 
           ((self fn res)
            (if (null? self) 
                res
                (foldl (cdr self) fn (fn (car self) res))))
           ((self fn)
            (if (number? (car self))
                (foldl self fn 0)
                (foldl self fn null)))))

        (define foldr
          (case-lambda
           ((self fn init-val)
            (foldl (reverse self) fn init-val))
           ((self fn)
            (foldl (reverse self) fn))))
        
	(define (flatten lst)
	  (if (not (null? lst))
	      (let loop ((args lst) (ret (list)))
		(if (not (null? args))
		    (let ((c (car args)))
		      (if (list? c)
			  (loop (cdr args) 
				(append ret (flatten c)))
			  (loop (cdr args) 
				(append ret (list c)))))
		    ret))
	      null))
	  
	(define list-remove-if null)

	(define (remove-if v p)
	  (list-remove-if v p))

	(define (remove-if-not v predic)
	  (list-remove-if v predic #t))

	(define find
	  (case-lambda
	   ((s v) (find s v 0 eq?))
	   ((s v offset) (find s v offset eq?))
	   ((s v b p)
	    (cond
	     ((null? s) #f)
	     (else
	      (let ((c null) (i b)
		    (len (length s)) (ret #f))
		(let loop ()
		  (when (< i len)
			(set! c (list-ref s i))
			(cond ((p v c)
			       (set! ret i))
			      (else
			       (set! i (add1 i))
			       (loop)))))
		ret))))))

	(define rfind
	  (case-lambda
	   ((s v) (rfind s v 0 eq?))
	   ((s v b) (rfind s v b eq?))
	   ((s v b p)
	    (cond
	     ((null? s) #f)
	     (else
	      (let ((r (find (reverse s) v b p)))
		(if (not (eq? r #f))
		    (begin
		      (set! r (add1 r))
		      (set! r (- (length s) r))))
		r))))))

	(define sort
	  (case-lambda
	   ((self) (sort self < 'sort))
	   ((self lt) (sort self lt 'sort))
	   ((self lt type)
	    (let ((f null))
	      (case type
		((sort) (set! f l::sort))
		((quick) (set! f l::quicksort))
		((merge) (set! f l::mergesort))
		(else (error "Invalid sort type.")))
	      (f self lt)))))

	(define merge-sorted
	  (case-lambda
	   ((list1 list2) (merge-sorted list1 list2 <))
	   ((list1 list2 lt)
	    (l::merge-sorted-lists list1 list2 lt))))

	(define (empty? self)
	  (l::empty? self))

	(define (filter self f)
	  (l::filter f self))

	(define (find-if self f)
	  (l::findf f self))

	(define unique
	  (case-lambda
	   ((self) (unique self eq?))
	   ((self cmpr)
	    (let ((ret (list)) (i null) (v #f))
	      (let loop ()
		(if (not (null? self))
		    (begin
		      (set! i (car self))
		      (if (eq? (find ret i 0 cmpr) #f)
			  (set! ret (append ret (list i))))
		      (set! self (cdr self))
		      (loop))))
	      ret))))

	(define unique?
	  (case-lambda 
	   ((self)
	    (unique? self eq?))
	   ((self cmpr)
	    (let ((len-f length))
	      (= (len-f self) (len-f (unique self cmpr)))))))

	(define remove 
	  (case-lambda 
	   ((self item)
	    (l::remove item self))
	   ((self item compr-proc)
	    (l::remove item self compr-proc))))

	;; Returns a new copy of lst after droping the first n elements in lst.
	(define (drop lst n)
	  (if (or (<= n 0) (null? lst))
	      lst
	      (drop (cdr lst) (- n 1))))

	;; Returns a new copy of lst that contains only the first n elements.
	(define (take lst n)
	  (if (or (<= n 0) (null? lst))
	      null
	      (cons (car lst) (take (cdr lst) (- n 1)))))

	;; Returns #t if predicate is true for all elements in elems.
	(define (every? elems predicate?)
	  (let/ec break
		  (let loop ((e elems))
		    (when (not (null? e))
			  (if (not (predicate? (car e)))
			      (break #f))
			  (loop (cdr e))))
		  #t))

	(define (find-all self predic)
	  (let loop ((tmp self) (res ()))
	    (if (not (null? tmp))
		(let ((v (car tmp)))
		  (if (predic v)
		      (loop (cdr tmp) (cons v res))
		      (loop (cdr tmp) res)))
		(reverse res))))
	
        ;; Checks if predic evaluates to #t when applied
        ;; for at least one of the elements in self.
	(define (some? self predic)
	  (let loop ((tmp self) (res #f))
	    (if (and (not (null? tmp))
		     (not res))
		(loop (cdr tmp) (predic (car tmp)))
		res)))

        ;; Replaces all instances of a with b in self.  Uses cmpr to 
        ;; compare each element in self with a.  cmpr defaults to eq?.
        (define replace-all
          (case-lambda
           ((self a b) (replace-helper self a b eq? #t))
           ((self a b cmpr) (replace-helper self a b cmpr #t))))

        ;; Replaces the first instance of a with b in self.  Uses cmpr to 
        ;; compare each element in self with a.  cmpr defaults to eq?
        (define replace
          (case-lambda
           ((self a b) (replace-helper self a b eq? #f))
           ((self a b cmpr) (replace-helper self a b cmpr #f))))

        (define (replace-helper self a b cmpr all?)
          (let loop ((s self) (replaced 0) (result ()))	
            (if (not (null? s))
                (let ((c (car s)))
                  (if (cmpr a c)
                      (if all?
                          (loop (cdr s) (add1 replaced) (cons b result))
                          (if (= replaced 0)
                              (loop (cdr s) (add1 replaced) (cons b result))
                              (loop (cdr s) replaced (cons c result))))
                      (loop (cdr s) replaced (cons c result))))
                (reverse result))))

	(set! list-remove-if
	      (lambda (v p . rev)
		(let ((ret (list)) (c null))
		  (let loop ()
		    (if (not (null? v))
			(begin
			  (set! c (car v))
			  (cond ((not (null? rev))
				 (if (p c) (set! ret (append ret (list c)))))
				(else
				 (if (not (p c)) (set! ret (append ret (list c))))))
			  (set! v (cdr v))
			  (loop))))
		  ret)))

	(provide foldl foldr flatten find rfind
		 remove-if remove-if-not remove
		 sort merge-sorted empty?
		 find-if filter unique
		 unique? drop take 
                 replace replace-all
		 every? some? find-all))