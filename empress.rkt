#! /usr/bin/env racket
#lang racket

(define (boomerspaces line)
  (string-replace line " " "  "))

(define (comma-splice line)
  (let ((splice? (random 2)))
    (cond
      [(and (not (string-contains? line ".\n"))
            (string-contains? line ".")
            (equal? splice? 1))
       (string-replace line "." ",")]
      [else line])))

(define (addatmosphere line)
  (let ((needsatmos? (random 2)))
    (cond
      [(equal? needsatmos? 1)
       (string-replace line "." "...")]
      [else line])))

(define (next-line-in-file file)
  (let ((line (read-line file 'any)))
    (unless (eof-object? line)
      (display (boomerspaces
                (comma-splice
                 (addatmosphere
                 (string-append line "\n")))))
      (next-line-in-file file))))

(define (empress fname)
  (cond
    [(file-exists? fname) (call-with-input-file fname next-line-in-file)]
    [else printf "no such file to enhance"]))

(empress (car (vector->list (current-command-line-arguments))))
