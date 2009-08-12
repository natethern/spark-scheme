(import (net)
	(reactor)
	(http)
	(aura))

(define httpd (web-server (list 'port 8080
				'session-timeout (* 2 60))))

(web-server-start httpd)
(web-server-stop httpd)