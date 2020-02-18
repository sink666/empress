#! /usr/bin/env racket
#lang racket

(define (boomerspaces line)
  (string-replace line " " "  "))

(define (comma-splice line)
  (cond
    [(and (not (string-contains? line ".\n"))
          (string-contains? line ".")
          (string-replace line "." ","))]
    [else line]))

(define (next-line-in-file file)
  (let ((line (read-line file 'any)))
    (unless (eof-object? line)
      (printf (boomerspaces
               (comma-splice line)))
      (next-line-in-file file))))

(define (empress fname)
  (cond
    [(file-exists? fname) (call-with-input-file fname next-line-in-file)]
    [else printf "no such file to enhance"]))

(empress (car (vector->list (current-command-line-arguments))))
