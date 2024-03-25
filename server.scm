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
;;;; Protocol
;;;

(define protocol "HTTP/1.1 ")

;;;
;;;; Server Name and version
;;;

(define server-version "beta 0.0.0")

(define server-name "Iron_Pig_WebServer")

(define server-name-and-version (string-append server-name " " server-version))

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
	(thread-start! (make-thread
			(lambda ()
			  (serve connection))))
	(loop)))))

;;;
;;;; Serve Connection
;;;

(define (serve connection)
  (let* ((headers (read-http-headers connection))
	 (request (car headers))
	 (parsed-path (parse-path (cadr headers))))
    (cond
     ((equal? request "GET") (serve-get connection parsed-path (pairize (list-tail headers 3))))
     ((equal? request "POST") (serve-post connection parsed-path (pairize (list-tail headers 3))))
     (#t (answer-404 connection)))
    (close-port connection)))

;;;
;;;; Read http headers
;;;

(define (read-http-headers connection)
  (let loop ((line (read-line connection)))
    (if (string=? line "") '()
	(append (split-string line #\space) (loop (read-line connection))))))
;;;
;;;; Activate Function
;;;

(define (activate-fnq fnq)
  (eval (string->symbol fnq)))

;;;
;;;; Parse Path
;;;

(define (parse-path path)    
  (let* ((nsf-pair (cdr (split-string path #\/)))
	 (ns (string-append (car nsf-pair) "#"))
	 (f-a (split-string (cadr nsf-pair) #\?))
	 (a (if (> (length f-a) 1) (split-string (cadr f-a) #\&) '()))
	 (args (flatten
		(map parse-args
		     (map (lambda (a)
			    (split-string a #\=))
			  a)))))
    (list->table `((fnq . ,(string-append ns (car f-a))) (args ,@args)))))

;;;
;;;; Parse Args
;;;

(define (parse-args ap)
  (cons (string->keyword (car ap)) (cdr ap)))

;;;
;;;;Answer-OK
;;;

(define (answer-OK connection type return #!key (code "200 OK\n"))
  (display (string-append
	    protocol code
	    "Content-Type: " type  "\n"
	    "Content-Length: " (number->string (string-length return))
	    "\r\n\r\n"
	    return)
	   connection))

;;;
;;;;Answer 204
;;;

(define (answer-204 connection)
  (display (string-append
	    protocol "204 No Content\n"
	    "Server: " server-name-and-version)
	   connection))

;;;
;;;; Answer 400
;;;

(define t-400 (make-table init: "Generic Error"))
(table-set! t-400 'unknown-content-type "Unsupported Content Type . . .")

(define (answer-400 connection reason)
  (let ((reason (table-ref t-400 reason)))
    (display (string-append
	      protocol  "400 Bad Request\n"
	      "Content-Type: text/plain\n"
	      "Content-Length: " (number->string (+ (string-length reason) 13))
	      "\r\n\r\n"
	      "Bad Request: " reason)
	     connection)))

;;;
;;;; Answer 404
;;;

(define (answer-404 connection)
  (display (string-append
	    protocol  "404 Not Found\n"
	    "Content-Type: text/plain\n"
	    "Content-Length: 9\r\n\r\n"
	    "Not Found")
	   connection))

;;;
;;;; Answer 411
;;;

(define (answer-411 connection)
  (display (string-append
	    protocol "411 Length Required\n"
	    "Server: " server-name-and-version
	    "\nContent-Type: plain/text\n"
	    "Content-Length: 23\r\n\r\n"
	    "Content Length Required")
	   connection))

;;;
;;;; Return Checker
;;;

(define (check-return connection fnq args)
  (with-exception-catcher
   (lambda (exc)
     'no-function-exists)
   (lambda ()
     (apply (activate-fnq fnq) args))))

;;;
;;;; Serve GET
;;;

(define (serve-get connection parsed-path server-info)
  (log "GET" parsed-path specifics: server-info)
  (let ((return (check-return connection (table-ref parsed-path 'fnq) (table-ref parsed-path 'args))))
    (cond
     ((table? return)
      (answer-OK connection "application/json" (table->json-string return)))
     ((and (string? return) (zero? (string-length return)))
      (answer-204 connection))
     ((or (string? return) (number? return))
      (let ((return-string (if (number? return) (number->string return) return)))
	(answer-OK connection "text/plain" return-string)))
     (#t (answer-404 connection)))))

;;;
;;;; Read Body
;;;

(define (read-body connection content-length)
  (if (zero? content-length) '()
      (cons (read-char connection) (read-body connection (- content-length 1)))))

;;;
;;;; Read String Number
;;;

(define (read-string-number body)
  (let ((number? (string->number body)))
    (if number? number? body)))

;;;
;;;; Serve POST
;;;

(define (serve-post connection parsed-path server-info)
  (log "POST" parsed-path specifics: server-info)
  (let ((content-length (assoc "Content-Length:" server-info)))
    (if content-length
	(let* ((content-type (cdr (assoc "Content-Type:" server-info)))
	       (content-length (string->number (cdr content-length)))
	       (body (list->string (read-body connection content-length)))
	       (args (cond
		      ((equal? content-type "application/json") (list (string->keyword "json-data") (json-string->table body)))
		      ((equal? content-type "text/plain") (list (string->keyword "plain") (read-string-number body)))
		      (#t #f))))
	  (if args
	      (let ((return (check-return connection (table-ref parsed-path 'fnq) args)))
		(cond
		 ((table? return)
		  (answer-OK connection  "application/json" (table->json-string return) code: "201 Created"))
		 ((string? return)
		  (answer-OK connection "text/plain" return code: "201 Created"))
		 ((number? return)
		  (answer-OK connection "text/plain" (number->string return)) code: "201 Created")
		 ((equal? #!void return)
		  (answer-OK conneciton "text/plain" "" code: "201 Created"))
		 ((equal? return 'no-function-exists) (answer-404 connection))))
	      (answer-400 connection 'unknown-content-type)))
	(answer-411 connection))))
					; Logging Framework
(define (log command parsed-path #!key (specifics '()))
  (with-output-to-file (list path: ".server_log" append: #t)
    (lambda ()
      (display
       (string-append (number->string (time->seconds (current-time))) ": \n"
		      (table-ref parsed-path 'fnq) " " (apply string-append (map (lambda (arg)
										   (if (keyword? arg) (keyword->string arg) arg)) (table-ref parsed-path 'args))) "\n" 
		      (apply string-append (map (lambda (pair)
						  (string-append (car pair) " " (cdr pair) "\n")) specifics))
		      "<=====================================================================================>"
		      "\r\n\r\n")))))
					; Debugging statements
(define (debug #!key plain)
  "SERVED")
