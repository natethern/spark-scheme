;; Some language extention trials. 
;; Copyright (C) 2007-2010 Vijay Mathew Pandyalakal

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

(module spark-lang-ext mzscheme

	(require library-manager)

	;; try-catch-finally
	(define-syntax try
	  (syntax-rules (catch finally)
	    ((_ try-body ... (catch catch-proc))
	     (with-handlers (((lambda (ex) #t)
			      (lambda (ex) 
				(catch-proc ex))))
			    (begin
			      try-body ...)))
	    ((_ try-body ... (catch catch-proc) (finally fin-body ...))
	     (dynamic-wind
		 (lambda () ())

		 (lambda ()
		   (with-handlers (((lambda (ex) #t)
				    (lambda (ex) 
				      (catch-proc ex))))
				  (begin
				    try-body ...)))
		 
		 (lambda () fin-body ...)))
	    ((_ try-body ... (finally fin-body ...))
	     (dynamic-wind
		 (lambda () ())
		 
		 (lambda () try-body ...)
		 
		 (lambda () fin-body ...)))))
	;; :~

	;; while loop

	(define-syntax while
	  (syntax-rules ()
	    ((while condition body ...)
	     (let __while_loop__ ()
	       (if (eq? condition #t)
		   (begin
		     body ...
		     (__while_loop__)))))))

	;; for loop on top of while loop

	(define-syntax for
	  (syntax-rules (in times)
	    ((for __v__ in __lst__ body ...)
	     (let* ((__v__ null)
		    (__list__ __lst__)
		    (__cond__ (lambda ()
				(if (eqv? __list__ null)
				    #f
				    (begin
				      (set! __v__ (car __list__))
				      (set! __list__ (cdr __list__))
				      #t)))))
	       (while (__cond__) body ...)))
	    ((for __x__ times body ...)
	     (let ((i 0))
	       (while (< i __x__)
		      body ...
		      (set! i (+ i 1)))))))


	;; A simplified module syntax

	(define-syntax library
	  (syntax-rules ()
	    ((library package-name package-body ...)
	     (module package-name spark
		     package-body ...))))

	(define-syntax export
	  (syntax-rules ()
	    ((export v ...)
	     (provide v ...))))

	
	(define-syntax import
	  (syntax-rules ()
	    ((_ (v1 ...) (v2 ...) ...)
	     (begin
	       (load-library 'v1 ...)
	       (load-library 'v2 ...)
	       ...
	       (require v1 ...)
	       (require v2 ...)
	       ...))))

	(define __sema__ (make-semaphore 1))
	(define __sema_map__ (make-hash-table 'equal))
	(define __sema_map_lock__ (make-semaphore 1))

	(define-syntax atomic
	  (syntax-rules ()
	    ((atomic block)
	     (let ((res null))
	       (semaphore-wait __sema__)
	       (set! res block)
	       (semaphore-post __sema__)
	       res))
	    ((atomic name block)
	     (let ((sema (hash-table-get __sema_map__ name null)))
	       (cond ((null? sema)
		      (set! sema (make-semaphore 1))
		      (semaphore-wait __sema_map_lock__)
		      (hash-table-put! __sema_map__ name sema)
		      (semaphore-post __sema_map_lock__)))
	       (let ((res null))
		 (semaphore-wait sema)
		 (set! res block)
		 (semaphore-post sema)
		 res)))))
	
	;; Simpler let.
	(define-syntax with
	  (syntax-rules ()
	    ((with () form . forms)
	     (begin form . forms))
	    ((with (s v . more) form . forms)
	     (let ((s v))
	       (with more form . forms)))))
	
	(provide try while for
		 atomic library export import
		 with))

