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

(web-server-hook! httpd 'before-handle-request client-timeout-hook)
(web-server-start httpd)
(web-server-stop httpd)