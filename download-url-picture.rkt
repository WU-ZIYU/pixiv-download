#lang at-exp racket
(require net/url)
(require racket/port)
(require racket/file)

(define (make-dir path)
  (displayln path)
  (make-directory* path))

(define (check-error web-string)
  (if (or (string-contains? web-string "404 Not Found")
          (string-contains? web-string "Error"))
      #t
      #f))


(define (download-pixiv-pictures param postfix target-destination base-url)  ; param is basically pixiv no.
  (define (format-url param postfix)
    (define url "~a/~a.~a")
    (format url base-url param postfix))
  (define (make-destination p)
    (printf "~a ~a ~a\n" p target-destination postfix)
    (define path-str (format "~a/~a.~a" target-destination p postfix))
    (if (path? (string->path target-destination))
        (begin
          (make-dir target-destination)
          path-str)
        (error (format "path ~a is invalid" path-str))))
  
  (define (download-picture url destination)
    (define in (get-pure-port (string->url url)))
    (define out (open-output-file destination #:exists 'replace))
    (copy-port in out) ; sed to copy the data from the input port (in) to the output port (out)
    (close-input-port in)
    (close-output-port out)
    (define picture-in (open-input-file destination))
    (define web-string (port->string picture-in))
    (if (check-error web-string)
        (begin
          (delete-file destination)
          #f)
        (begin
          (printf "download ~a success\n" url)
          #t)))
  
  (let loop ([i 1])
    (define param-i (format "~a-~a" param i))
    (define destination-i (make-destination param-i))
    (define url-i (format-url param-i postfix))
    (if (download-picture url-i destination-i)
        (if (= i 100) null
            (loop (add1 i)))
        (if (= i 1)
            (let ([destination (make-destination param)]
                  [url (format-url param postfix)])
              (if (download-picture url destination)
                  (displayln "download end")
                  (error (format "url ~a not found" url))))
            (displayln "download end")))))

(provide download-pixiv-pictures)