#! /usr/bin/env racket
#lang racket

(define (next-line-in-file file)
  (let ((line (read-line file 'any)))
    (unless (eof-object? line)
      (printf line)
      (next-line-in-file file))))

(define (empress fname)
  (cond
    [(file-exists? fname) (call-with-input-file fname next-line-in-file)]
    [else printf "no such file to enhance"]))

(empress (car (vector->list (current-command-line-arguments))))
