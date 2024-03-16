;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                                                                        ;;;
;;;     __  .______     ______   .__   __.    .______    __    _______.                    ;;;
;;;    |  | |   _  \   /  __  \  |  \ |  |    |   _  \  |  |  /  _____|        _____       ;;;     
;;;    |  | |  |_)  | |  |  |  | |   \|  |    |  |_)  | |  | |  |  __      ^..^     \9     ;;;
;;;    |  | |      /  |  |  |  | |  . `  |    |   ___/  |  | |  | |_ |     (oo)_____/      ;;;
;;;    |  | |  |\  \  |  `--'  | |  |\   |    |  |      |  | |  |__| |        WW  WW       ;;;
;;;    |__| | _| `._|  \______/  |__| \__|    | _|      |__|  \______|                     ;;;
;;;                                                                                        ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;
;; Author: Ivan Jordaan
;; Date: 16/03/'24
;; email: ivan@axoinvent.com
;; Project: Web Server
;;
     

;;;
;;;; Server Loop
;;;

(define (server)
  (let ((server (open-tcp-server (list local-address: "*"
				       local-port-number: 8080
				       eol-encoding: 'cr-lf
				       reuse-address: #t))))
    (println "Server started on loopback address")
    (let loop ()
      (let ((connection (read server)))
	(thread-start! (make-thread
			(lambda ()
			  (serve connection))))
	(loop)))))

;;;
;;;; Serve Connection
;;;
  
(define (serve connection)
  (let loop ((line (read-line connection)))
    (if (string=? "" line)
	(begin
	  (display http-response connection)
	  (force-output connection)
	  (close-port connection))
	(begin
	  (display line)
	  (loop (read-line connection))))))

(define (read-http-headers connection)
  (let loop ((line (read-line connection)))
    (if (string=? line "") '()
	(append (string-split line #\space) (loop (read-line connection))))))


(define http-response
  "HTTP/1.1 200 OK\r\nContent-Type: text/plain\r\n\r\nHello, world!\r\n")
;;;
;;;; Server Start
;;;

(server)
