#lang at-exp racket/gui
(require "./download-url-picture.rkt")

(define WIDTH 200)
(define url-header "https://")
(define pixiv-url "pixiv.re")
(define iili-url "iili.io")

(define (empty-string? str)
  (not (non-empty-string? str)))

(define main-frame
  (new frame%
       [label "Download pixiv"]
       [width WIDTH]))

(define main-panel (new vertical-panel%
                        [parent main-frame]))

; define two horizontal panel for control buttons and texts
(define control-panel1
  (new horizontal-panel%
       [parent main-panel]))

(define control-panel2
  (new horizontal-panel%
       [parent main-panel]))

(define control-panel3
  (new horizontal-panel%
       [parent main-panel]))

; the upper panel: control-panel 1
(define btn-reset
  (new button%
       [parent control-panel1]
       [label "Reset"]
       [callback (λ (button event)
                   (reset))]))

; the down panel: control-panel 2
(define btn-download
  (new button%
       [parent control-panel2]
       [label "Download"]
       [callback (λ (button event)
                   (download))]))

; text field to input pixiv no.
(define text-pixiv-no
  (new text-field%
       [parent control-panel1]
       [init-value ""]
       [label "pixiv no.: "]
       [callback (λ (text-field evt)
                   (define path (generate-default-destination (get-pixiv-no)))
                   (if set-destination
                       '()
                       (send text-pixiv-destination set-value path)))]))

(define (get-pixiv-no)
  (send text-pixiv-no get-value))

(define (set!-pixiv-no-empty)
  (send text-pixiv-no set-value ""))

; text field to input pixiv file download destination
(define set-destination #f)
(define text-pixiv-destination
  (new text-field%
       [parent control-panel2]
       [init-value ""]
       [label "pixiv download to: "]
       [callback (λ (text-field evt)
                   (define path (get-pixiv-destination))
                   (if (string=? "" path)
                       (set! set-destination #f)
                       (set! set-destination #t)))]))

(define (generate-default-destination pixiv-no)
  (format "./pixiv-pictures/~a" pixiv-no))

(define (get-pixiv-destination)
  (send text-pixiv-destination get-value))

(define (set!-pixiv-destination-empty)
  (send text-pixiv-destination set-value ""))

; the url panel: control-panel 3
(define btn-set-default-url
  (new button%
       [parent control-panel3]
       [label "Reset url"]
       [callback (λ (button event)
                   (set!-url-default))]))

; text field to input url, if not pixiv
(define text-url
  (new text-field%
       [parent control-panel3]
       [init-value pixiv-url]
       [label "url (default pixiv): "]
       [callback (λ (text event)
                   (let ([now-url (get-url)])
                     (if (and (not (string=? now-url pixiv-url))
                              (not (string=? now-url iili-url)))
                         (send choice-url set-selection 2)
                         null)))]))

(define (get-url)
  (send text-url get-value))

(define (set!-url-default)
  (send text-url set-value pixiv-url))

(define (set!-url url)
  (send text-url set-value url))

; choice to choose sync or async
(define choice-sync
  (new choice%
       [parent control-panel1]
       [label "sync or async download: "]
       [choices '("sync" "async")]))

(define (get-sync-choice)
  (send choice-sync get-string-selection))

; choice to choose jpg or png format
(define choice-pixiv-format
  (new choice%
       [parent control-panel1]
       [label "pixiv format: "]
       [choices '("jpg" "png")]))

(define (get-format-choice)
  (send choice-pixiv-format get-string-selection))

; choice to choose pixiv or iili format
(define choice-url
  (new choice%
       [parent control-panel1]
       [label "pixiv/iili/others: "]
       [choices '("pixiv" "iili" "others")]
       [callback (lambda (choice event)
                   (let ([now-url-choice (get-url-choice)])
                     (cond [(string=? now-url-choice "pixiv") (set!-url-default)]
                           [(string=? now-url-choice "iili") (set!-url iili-url)]
                           [(string=? now-url-choice "others") null])))])) ; "others" does nothing as url has been input in text-url panel

(define (get-url-choice)
  (send choice-url get-string-selection))

(define (reset)
  (set!-pixiv-no-empty)
  (set!-pixiv-destination-empty)
  (set!-url-default))


; set message frame
(define message-frame
  (new frame%
       [label "Error"]
       [width WIDTH]))

(define dialog (new message%
                    [parent message-frame]
                    [label "pixiv no. or pixiv destination must not be empty!"]))

(define message-basic-panel
  (new vertical-panel%
       [parent message-frame]))

(define message-panel
  (new horizontal-panel%
       [parent message-basic-panel]))

(define btn-hide-message
  (new button%
       [parent message-panel]
       [label "OK"]
       [callback (λ (button event)
                   (send message-frame show #f))]))


(define (download)
  (define pixiv-no (get-pixiv-no))
  (define pixiv-destination (get-pixiv-destination))
  (define pixiv-format-choice (get-format-choice))
  (define url (format "~a~a" url-header (get-url)))
  (if (or (empty-string? pixiv-no)
          (empty-string? pixiv-destination))
      (begin
        (send message-frame show #t)
        (reset))
      ; download sync or async
      (if (string=? (get-sync-choice) "async")
          (thread (lambda ()
                    (download-pixiv-pictures pixiv-no pixiv-format-choice pixiv-destination url)))
          (download-pixiv-pictures pixiv-no pixiv-format-choice pixiv-destination url))))

(send main-frame show #t)

; https://pixiv.re
