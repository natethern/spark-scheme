(import (aura))

(define (get-num1 new-uri http)
  (let ((html (sgml `(html
		      (body
		       (form ((action ,new-uri))
			     "Enter number 1: "
			     (input ((type "text")
				     (name "num1")))))))))
    ((html 'text))))

(define (get-num2 new-uri http)
  (let ((html (sgml `(html
		      (body
		       (form ((action ,new-uri))
			     "Enter number 2: "
			     (input ((type "text")
				     (name "num2")))))))))
    ((html 'text))))

(define (add new-uri http)
  (let ((s1 (http-value http "num1"))
	(s2 (http-value http "num2")))

    ;; If a value is not a number, go back to
    ;; the appropriate page and request a new value.
    (if (not (number? (string->number s1)))
	(http-call get-num1))
    (if (not (number? (string->number s2)))
	(http-call get-num2))

    ;; Make this session re-usable.
    ;(http-keep-alive! http)

    (let* ((res (number->string (+ (string->number s1)
				   (string->number s2))))
	   (html (sgml `(html
			 (body
			  (b ,res))))))
      ((html 'text)))))

(list get-num1 get-num2 add)