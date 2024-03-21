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
;;;;
;;;

(define server-name "Iron_Pig_WebServer")

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
	;;(thread-start! (make-thread
	;;(lambda ()
	(serve connection)
	;;)))
	(loop)))))

;;;
;;;; Serve Connection
;;;

(define (serve connection)
					; Read Headers
  (define (read-http-headers)
    (let loop ((line (read-line connection)))
      (if (string=? line "") '()
	  (append (split-string line #\space) (loop (read-line connection))))))
					; Activate Function
  (define (activate-fnq fnq)
    (eval (string->symbol fnq)))
					; Parse Path
  (define (parse-path path)
					; Parse Args
    (define (parse-args ap)
      (cons (string->keyword (car ap)) (cdr ap)))
    
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
					; Answer 200/1
  
  (define (answer-OK type return #!key (code "200 OK\n"))
    (display (string-append
	      protocol code
	      "Content-Type: " type  "\n"
	      "Content-Length: " (number->string (string-length return)) "\r\n\r\n"
	      return)
	     connection))
    					; Answer 204
  (define (answer-204)
    (display (string-append
	      protocol "204 No Content\n"
	      "Server: " server-name)
	     connection))
					; Answer 404
  (define (answer-404)
    (display (string-append
	      protocol  "404 Not Found\n"
	      "Content-Type: text/plain\n"
	      "Content-Length: 13\r\n\r\n"
	      "Not Found")
	     connection))
					; Serve GET
  (define (serve-get parsed-path)
    (let ((return (apply (activate-fnq (table-ref parsed-path 'fnq)) (table-ref parsed-path 'args))))
      (log "GET" parsed-path)
      (cond
       ((table? return)
	(answer-OK "application/json" (table->json-string return)))
       ((and (string? return) (zero? (string-length return)))
	(answer-204))
       ((or (string? return) (number? return))
	(let ((return-string (if (number? return) (number->string return) return)))
	  (answer-200 "text/plain" return-string)))
       (#t (answer-404)))))
					; Serve POST
  (define (serve-post parsed-path server-info)
    (let ((host (cadr server-info))
	  (user-agent (cadr (list-tail server-info 2)))
	  (accept (cadr (list-tail server-info 4))))
      (log "POST" parsed-path "host: " host "\nUser-Agent: " user-agent)
      (answer-OK "text/plain" "Success" code: "201 Created\n")))
  
  (let* ((headers (read-http-headers))
	 (request (car headers))
	 (parsed-path (parse-path (cadr headers))))
    (cond
     ((equal? request "GET") (serve-get parsed-path))
     ((equal? request "POST") (serve-post parsed-path (list-tail headers 3)))
     (#t (answer-404)))
    (close-port connection)))

(define (log command parsed-path . specifics)
  (with-output-to-file (list path: ".server_log" append: #t)
    (lambda ()
      (display
       (string-append (number->string (time->seconds (current-time))) ": \n"
		      (table-ref parsed-path 'fnq) " " (apply string-append (table-ref parsed-path 'args)) "\n" 
		      (apply string-append specifics))))))

(define (debug)
  (#t))

(server)
