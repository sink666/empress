#! /usr/bin/env racket
#lang racket

;;improve spacing
(define (alter-spacing line)
  (let ((booming? (random 2)))
    (cond
      ((equal? booming? 1)
       (string-replace line " " "  "))
      (else (string-replace line "." ". ")))))

;;check for line breaks and places for splices
(define check-line
  (lambda (line)
    (condz
      ((and (not (string-contains? line ".\n"))
           (string-contains? line "."))))))

;;introduce comma splices and more atmospheric comma splices
(define improve-grammar
  (lambda (line)
    (let ((splice? (random 2)) (atmos-splice? (random 2)))
      (cond
        ((and (check-line line)
              (equal? atmos-splice? 1))
         (string-replace line "." "..."))
        ((and (check-line line)
              (eof-object? line)
              (equal? splice? 1))
         (string-replace line "." ","))
        (else line)))))

(define process-line
  (lambda (line)
    (improve-grammar (alter-spacing (string-append line "\n")))))

(define enhance-file
  (lambda (fn)
    (let ((line (read-line fn 'any)))
      (unless (eof-object? line)
        (display (process-line line))
        (enhance-file fn)))))

;;add filename suggestions to the last else?
(define empress
  (lambda (cla)
    (cond
      ((vector-empty? cla) (printf "err; no filename given"))
      (else
       (let ((fn (vector-ref cla 0)))
         (cond
           ((file-exists? fn) (call-with-input-file fn enhance-file))
           (else (printf "err; no file with name ~a" fn))))))))

(printf "enhancing file!\n\n")
(empress (current-command-line-arguments))

;;eof
