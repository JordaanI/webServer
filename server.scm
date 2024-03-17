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
    (println "Server Started")
    (let loop ()
      (let ((connection (read server)))
	;(thread-start! (make-thread
			;(lambda ()
	(serve connection)
	;)))
	(loop)))))

;;;
;;;; Serve Connection
;;;
  
(define (serve connection)

  (define (serve-get-to path) ;should parse path and eval it. That eval should return a string that can be asnwered to request, should contain content type and return string
    (let ((parsed-path (parse-path path))) 
      (with-output-to-file
	  (list path: ".serverlog"
		append: #t)
	(lambda ()
	  (display (time->seconds (current-time)))
	  (display ":\n")
	  (display (table->list parsed-path))
	  (display "\r\n\r\n")))
       ((eval (string->symbol (table-ref parsed-path 'fnq))))))

  (define (answer protocol code return)
    (display (string-append
	      protocol " "
	      code
	      "\r\nContent-type: application/json\r\n\r\n"
	      return)
	     connection)
    (close-port connection))

  (let* ((http-header (read-http-headers connection))
	 (path (cadr http-header))
	 (protocol (caddr http-header)))
    (cond
     ((equal? (car http-header) "GET")
      (answer protocol "200 OK" (serve-get-to path)))
     (#t (raise 'unknown-command)))))

(define (read-http-headers connection)
  (let loop ((line (read-line connection)))
    (if (string=? line "") '()
	(append (split-string line #\space) (loop (read-line connection))))))

;;;
;;;;Parse Path
;;;

(define (parse-path path)
  (let* ((nsf-pair (cdr (split-string path #\/)))
	 (ns (string-append (car nsf-pair) "#"))
	 (f-a (split-string (cadr nsf-pair) #\?))
	 (a (if (> (length f-a) 1) (split-string (cadr f-a) #\&) '()))
	 (args (map (lambda (a)
		      (split-string a #\=))
		    a)))
    (list->table `((fnq . ,(string-append ns (car f-a))) (args ,@args)))))


(define (test)
  "{}")

;;;
;;;;Start Server
;;;

(server)

