(import (net)
	(http))
	
(define httpd (web-server (list 'port 8080
				'session-timeout (* 2 60))))

;; This is a 'before-handle-request hook
;; that will watch each client handler thread
;; for a timeout. On timeout, if the thread
;; is still running, it is killed and the socket
;; is closed.
(define (client-timeout-hook web-server-obj
			     client-connection
			     http-request)
  (let ((thrd (current-thread)))
    (thread (lambda ()
	      ;; Keep an alarm for 5 seconds.
	      (sync (alarm-evt (+ (current-inexact-milliseconds)
				  (* 5 1000))))
	      (cond 
	       ((thread-running? thrd)
		(write-log web-server-obj 					  
			   (list "Terminating thread ~a on timeout."
				 thrd))
		(kill-thread thrd)
		(try
		 (socket-close (connection-socket client-connection))
		 (catch (lambda (error)
			  (write-log web-server-obj
				     '("Error: (socket-close): ~a."
				       error)))))))))
    #t))

;; A hook that will be executed before sending each response.
(define (reponse-hook web-server-obj client-connection response)
  (printf "~a~n" response) (flush-output))

;; Set the hooks.
(web-server-hook! httpd 'before-handle-request client-timeout-hook)
(web-server-hook! httpd 'before-send-response reponse-hook)

(define conn-count 0)

;; Start the server with a simple exit condition - serve upto 10000 connection.
(web-server-start httpd 
		  (lambda () 
		    (set! conn-count (add1 conn-count)) 
		    (< conn-count 10000)))

(web-server-stop httpd)