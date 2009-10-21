;; Some list and vector utilities. 
;; Copyright (C) 2007, 2008, 2009  Vijay Mathew Pandyalakal

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

	(define list-flatten null)
	(define vector-flatten null)
	
	(define (flatten args)
	  (cond 
	   ((list? args)
	    (list-flatten args))
	   ((vector? args)
	    (vector-flatten args))
	   (else
	    (error "Could not flatten this object."))))

	(define list-remove-if null)
	(define vector-remove-if null)

	(define (remove-if v p)
	  (cond 
	   ((list? v)
	    (list-remove-if v p))
	   ((vector? v)
	    (vector-remove-if v p))
	   (else
	    (error "(remove-if) cannot be applied on this object."))))

	(define (remove-if-not v predic)
	  (cond 
	   ((list? v)
	    (list-remove-if v predic #t))
	   ((vector? v)
	    (vector-remove-if v predic #t))
	   (else
	    (error "(remove-if-not) cannot be applied on this object."))))

	(define find
	  (case-lambda
	   ((s v) (find s v 0 eq?))
	   ((s v offset) (find s v offset eq?))
	   ((s v b p)
	    (cond
	     ((null? s) #f)
	     (else
	      (if (not (list? s))
		  (begin
		    (cond 
		     ((vector? s) (set! s (vector->list s)))
		     ((string? s) (set! s (string->list s)))
		     (else (error "Cannot do a find on this object.")))))
	      (let ((c null) (i b)
		    (len (length s)) (ret #f))
		(let loop ()
		  (if (< i len)	    
		      (begin
			(set! c (list-ref s i))
			(if (p v c)
			    (set! ret i)
			    (begin
			      (set! i (add1 i))
			      (loop))))))
		ret))))))

	(define rfind
	  (case-lambda
	   ((s v) (rfind s v 0 eq?))
	   ((s v b) (rfind s v b eq?))
	   ((s v b p)
	    (cond
	     ((null? s) #f)
	     (else
	      (if (not (list? s))
		  (begin
		    (cond 
		     ((vector? s) (set! s (vector->list s)))
		     ((string? s) (set! s (string->list s)))
		     (else (error "Cannot do a rfind on this object.")))))
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
	      (cond
	       ((list? self) (f self lt))
	       ((vector? self) (list->vector (f (vector->list self) lt)))
	       (else (error "Cannot sort this type.")))))))

	(define merge-sorted
	  (case-lambda
	   ((list1 list2) (merge-sorted list1 list2 <))
	   ((list1 list2 lt)
	    (cond
	     ((and (list? list1) (list? list2))
	      (l::merge-sorted-lists list1 list2 lt))
	     ((and (vector? list1) (vector? list2))
	      (list->vector (l::merge-sorted-lists (vector->list list1) 
						   (vector->list list2) lt)))
	     (else (error "Cannot merge-sort this type."))))))
	
	(define (empty? self)
	  (cond
	   ((list? self) (l::empty? self))
	   ((vector? self) (l::empty? (vector->list self)))
	   (else (error "Invalid type."))))

	(define (filter self f)
	  (cond
	   ((list? self) (l::filter f self))
	   ((vector? self) (list->vector (l::filter f (vector->list self))))
	   (else (error "Invalid type."))))

	(define (find-if self f)
	  (cond
	   ((list? self) (l::findf f self))
	   ((vector? self) (l::findf f (vector->list self)))
	   (else (error "Invalid type."))))

	(define unique
	  (case-lambda
	   ((self) (unique self eq?))
	   ((self cmpr)
	    (let ((ret (list)) (i null) (v #f))
	      (if (vector? self)
		  (begin
		    (set! self (vector->list self))
		    (set! v #t)))
	      (let loop ()
		(if (not (null? self))
		    (begin
		      (set! i (car self))
		      (if (eq? (find ret i 0 cmpr) #f)
			  (set! ret (append ret (list i))))
		      (set! self (cdr self))
		      (loop))))
	      (if v
		  (list->vector ret)
		  ret)))))

	(define unique?
	  (case-lambda ((self)
			(unique? self eq?))
		       ((self cmpr)
			(let ((len-f length))
			  (if (vector? self)
			      (set! len-f vector-length))
			  (= (len-f self) (len-f (unique self cmpr)))))))

	(define remove 
	  (case-lambda ((self item)
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
	  (if (not (list? elems))
	      (cond 
	       ((vector? elems) (set! elems (vector->list elems)))
	       ((string? elems) (set! elems (string->list elems)))
	       (else (error "(every? cannot be applied to this type."))))
	  (let/ec break
		  (let loop ((e elems))
		    (if (not (null? e))
			(begin
			  (if (not (predicate? (car e)))
			      (break #f))
			  (loop (cdr e)))))
		  #t))

	(define (find-all self predic)
	  (let loop ((tmp self) (res ()))
	    (if (not (null? tmp))
		(let ((v (car tmp)))
		  (if (predic v)
		      (loop (cdr tmp) (cons v res))
		      (loop (cdr tmp) res)))
		(reverse res))))
	
	(define (some? self predic)
	  (let loop ((tmp self) (res #f))
	    (if (and (not (null? tmp))
		     (not res))
		(loop (cdr tmp) (predic (car tmp)))
		res)))

	(set! list-flatten 
	      (lambda (lst)
		(if (not (null? lst))
		    (let loop ((args lst) (ret (list)) (c null))
		      (if (not (null? args))
			  (begin
			    (set! c (car args))
			    (cond ((list? c) 
				   (loop (cdr args) 
					 (append ret (list-flatten c)) c))
				  (else 
				   (loop (cdr args) 
					 (append ret (list c)) c))))
			  ret))
		    null)))

	(set! vector-flatten
	      (lambda (args)
		(let ((ret (list))
		      (c null)
		      (len (vector-length args))
		      (i 0))
		  (let loop ()
		    (if (< i len)
			(begin
			  (set! c (vector-ref args i))
			  (if (vector? c)
			      (set! ret (append ret (vector->list (vector-flatten c))))
			      (set! ret (append ret (list c))))
			  (set! i (add1 i))
			  (loop))))
		  (list->vector ret))))

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

	(set! vector-remove-if
	      (lambda (v p . rev)
		(if (not (null? rev))
		    (list->vector (list-remove-if (vector->list v) p (car rev)))
		    (list->vector (list-remove-if (vector->list v) p)))))

	(provide flatten find rfind
		 remove-if remove-if-not remove
		 sort merge-sorted empty?
		 find-if filter unique
		 unique? drop take
		 every? some? find-all))