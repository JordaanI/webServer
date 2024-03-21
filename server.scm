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
	;(thread-start! (make-thread
			;(lambda ()
	(serve connection)
	;)))
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
      (list->table `((fnq . ,(eval (string->symbol (string-append ns (car f-a))))) (args ,@args)))))
					; Serve GET
  (define (serve-get parsed-path)
					; Answer 200
    (define (answer-200 type string)
      (display (string-append
		protocol "200 OK\n"
		"Content-Type: " type
		"\nContent-Length: " (number->string (string-length string)) "\r\n\r\n"
		string)
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
		"HTTP/1.1 404 Not Found\n"
		"Content-Type: text/plain\n"
		"Content-Length: 13\r\n\r\n"
		"Not Found")
	       connection))
    
    (let ((return (apply (table-ref parsed-path 'fnq) (table-ref parsed-path 'args))))
      (cond
       ((table? return)
	(let ((json-string (table->json-string return)))
	  (answer-200 "application/json" json-string)))
       ((and (string? return) (zero? (string-length return)))
	(answer-204))
       ((or (string? return) (number? return))
	(let ((return-string (if (number? return) (number->string return) return)))
	  (answer-200 "text/plain" return-string)))
       (#t (answer-404)))))

  
  (let* ((headers (read-http-headers))
	 (request (car headers))
	 (parsed-path (parse-path (cadr headers))))
    (cond
     ((equal? request "GET") (serve-get parsed-path))
     ((equal? request "POST") (serve-post parsed-path (read-line connection)))
     (#t (answer-404)))
    (close-port connection)))

