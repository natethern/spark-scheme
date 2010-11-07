(import (sunit))

(define verbose #f)

(define (length-using-foldn lst)
  (foldl (replace-all lst null 1 (lambda (x y) #t)) +))

(sunit-tests "(foldl)"
	     (list 
	      (make-test '(foldl (list 1 2 3 4 5) +) 15)
	      (make-test '(foldl (list 1 2 3 4 5) + 10) 25)
	      (make-test '(foldl (list 1 2 3 4 5) cons ()) (list 5 4 3 2 1))
	      (make-test '(foldr (list 1 2 3 4 5) cons ()) (list 1 2 3 4 5))
	      (make-test '(foldl (list 1 2 3 4 5) cons ()) (list 5 4 3 2 1))
	      (make-test '(length-using-foldn (list 'a 'b 'c)) 3))
	     verbose)

(sunit-tests "(flatten)"
	     (list 
	      (make-test '(flatten (list 1 2 (list 3 4))) (list 1 2 3 4))
	      (make-test '(flatten (list 1 2 (list 3 4 (list "sunny") (list 'a 'b 'c)))) 
			 (list 1 2 3 4 "sunny" 'a 'b 'c))
	      (make-test '(flatten (list 1 2 3 4)) (list 1 2 3 4))
	      (make-test '(flatten (list)) (list))
	      (make-test '(flatten (vector->list (vector 1 2 3 4))) (list 1 2 3 4)))
	     verbose)

(define (cmp a)
  (= a 20))

(sunit-tests "(remove-if)"
	     (list
	      (make-test '(remove-if (list 10 20 30 40 50) cmp) (list 10 30 40 50))
	      (make-test '(remove-if (list 10 20 30 20 40 50) cmp) (list 10 30 40 50))
	      (make-test '(remove-if (list 10 30 40 50) cmp) (list 10 30 40 50))
	      (make-test '(remove-if (list) cmp) (list)))
	     verbose)

(sunit-tests "(remove-if-not)"
	     (list
	      (make-test '(remove-if-not (list 10 20 30 40 50) cmp) (list 20))
	      (make-test '(remove-if-not (list 10 20 30 20 40 50) cmp) (list 20 20))
	      (make-test '(remove-if-not (list 10 30 40 50) cmp) (list))
	      (make-test '(remove-if-not (list) cmp) (list)))
	     verbose)


(sunit-tests "(find)"
	     (list
	      (make-test '(find (list 10 20 30 100 50) 30) 2)
 	      (make-test '(find (list 10 20 30 100 50) 30 2) 2)
 	      (make-test '(find (list 10 20 30 100 50) 30 5) #f)
 	      (make-test '(find (list 10 20 30 100 50) 100) 3)
 	      (make-test '(find (list 10 20 30 100 50) 10) 0)
 	      (make-test '(find (list 10 20 30 100 50) 50) 4)
 	      (make-test '(find (list 10 20 30 100 50) 300) #f)
 	      (make-test '(find (list) 30) #f)
 	      (make-test '(find (list "sunny" "jenny" "crystel") "jenny" 0 equal?) 1)
 	      (make-test '(find (list "sunny" "jenny" "crystel" "kenny") "crystel" 1 equal?) 2)
 	      (make-test '(find (list "sunny" "jenny" "crystel" "kenny") "sam" 1 equal?) #f)
 	      (make-test '(find (list) "sam") #f)
	      (make-test '(find (list "sam") "sam" 0 equal?) 0)
	      (make-test '(find (list "sam") "sam" 1 equal?) #f))
	     verbose)

(sunit-tests "(rfind)"
	     (list
	      (make-test '(rfind (list 10 20 30 100 50) 30) 2)
 	      (make-test '(rfind (list 10 20 30 100 50) 30 2) 2)
 	      (make-test '(rfind (list 10 20 30 100 50) 30 5) #f)
 	      (make-test '(rfind (list 10 20 30 100 50) 100) 3)
 	      (make-test '(rfind (list 10 20 30 100 50) 10) 0)
 	      (make-test '(rfind (list 10 20 30 100 50) 50) 4)
 	      (make-test '(rfind (list 10 20 30 100 50) 300) #f)
 	      (make-test '(rfind (list) 30) #f)
 	      (make-test '(rfind (list "sunny" "jenny" "crystel") "jenny" 0 equal?) 1)
 	      (make-test '(rfind (list "sunny" "jenny" "crystel" "kenny") "crystel" 1 equal?) 2)
 	      (make-test '(rfind (list "sunny" "jenny" "crystel" "kenny") "sam" 1 equal?) #f)
 	      (make-test '(rfind (list) "sam") #f)
	      (make-test '(rfind (list "sam") "sam" 0 equal?) 0)
	      (make-test '(rfind (list "sam") "sam" 1 equal?) #f))
	     verbose)

(sunit-tests "(find-all)"
	     (list
	      (make-test '(find-all (list 10 20 30 40 50) (lambda (x) (>= x 20))) (list 20 30 40 50))
	      (make-test '(find-all (list 10 20 30 40 50) (lambda (x) (>= x 200))) ())
	      (make-test '(find-all () (lambda (x) (>= x 200))) ())
	      (make-test '(find-all (list 1 2 3 4) (lambda (x) (= x 1))) (list 1))
	      (make-test '(find-all (list 1 2 3 4) (lambda (x) (= x 4))) (list 4))
	      (make-test '(find-all (list "a" "b" "c" "d") (lambda (x) (or (string=? x "a") (string=? x "c"))))
			 (list "a" "c")))
	     verbose)

(sunit-tests "(empty?)"
	     (list
	      (make-test '(empty? (list 10 20 30 100 50)) #f)
	      (make-test '(empty? (list)) #t))
	     verbose)

(sunit-tests "(some?)"
	     (list
	      (make-test '(some? (list 10 20 30 40 50) (lambda (x) (>= x 20))) #t)
	      (make-test '(some? (list 10 20 30 40 50) (lambda (x) (>= x 200))) #f)
	      (make-test '(some? () (lambda (x) (>= x 200))) #f)
	      (make-test '(some? (list 1 2 3 4) (lambda (x) (= x 1))) #t)
	      (make-test '(some? (list 1 2 3 4) (lambda (x) (= x 4))) #t)
	      (make-test '(some? (list "a" "b" "c" "d") (lambda (x) (or (string=? x "a") (string=? x "c"))))
			 #t))
	     verbose)

(sunit-tests "(sort)"
	     (list 
	      (make-test '(sort (list 20 30 10 100 50)) (list 10 20 30 50 100))
	      (make-test '(sort (list "abc" "zzse" "aaa" "qas") string<?) (list "aaa" "abc" "qas" "zzse")))
	     verbose)

(define (f x)
  (if (= x 100)
      #f
      #t))

(sunit-tests "(filter)"
	     (list 
	      (make-test '(filter '(20 30 40 100 10 50 100) f) '(20 30 40 10 50)))
	     verbose)

(sunit-tests "(unique)"
	     (list
	      (make-test '(unique '(10 20 30 20 100 4 5 20 5)) '(10 20 30 100 4 5))
	      (make-test '(unique '("hello" "bye" "is" "bye" "true") string=?) '("hello" "bye" "is" "true"))
	      (make-test '(unique '()) '()))
	     #f)

(sunit-tests "(unique?)"
	     (list
	      (make-test '(unique? '(10 20 30 20 100 4 5 20 5)) #f)
	      (make-test '(unique? '(10 20 30 100 4 5)) #t)
	      (make-test '(unique? ()) #t)
	      (make-test '(unique? '("hello" "bye" "is" "bye" "true") string=?) #f)
	      (make-test '(unique? '("hello" "bye" "is" "true") string=?) #t))
	     #f)

(sunit-tests "(drop)"
	     (list
	      (make-test '(drop '(1 2 3 4 5 6) 3) '(4 5 6))
	      (make-test '(drop '(1 2 3 4 5 6) 1) '(2 3 4 5 6))
	      (make-test '(drop '(1 2 3 4 5 6) 0) '(1 2 3 4 5 6))
	      (make-test '(drop '(1 2 3 4 5 6) -2) '(1 2 3 4 5 6))
	      (make-test '(drop null 3) null)
	      (make-test '(drop null -3) null)
	      (make-test '(drop '(1) 3) null)
	      (make-test '(drop '(1 2 3) 3) null)
	      (make-test '(drop '(1) 1) null))
	     #f)

(sunit-tests "(take)"
	     (list
	      (make-test '(take '(1 2 3 4 5 6) 3) '(1 2 3))
	      (make-test '(take '(1 2 3 4 5 6) 1) '(1))
	      (make-test '(take '(1 2 3 4 5 6) 0) null)
	      (make-test '(take '(1 2 3 4 5 6) -2) null)
	      (make-test '(take null 3) null)
	      (make-test '(take null -3) null)
	      (make-test '(take '(1) 3) '(1))
	      (make-test '(take '(1 2 3) 3) '(1 2 3))
	      (make-test '(take '(1) 1) '(1)))
	     #f)
