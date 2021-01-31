#!/bin/racket
#lang racket

(define (boomer-spacing line)
  (let ([booming? (random 2)])
    (cond
      [(eq? booming? 1)
       (string-replace line " " "  ")]
      [else
       (string-replace line "." ". ")])))

(define (check-line line)
  (and (not (string-contains? line ".\n"))
        (string-contains? line ".")))

(define (improve-grammar line)
  (let ([splice? (random 2)] [atmos-splice? (random 2)])
    (cond
      [(and (eq? atmos-splice? 1)
            (check-line line))
       (string-replace line "." "...")]
      [(and (eq? splice? 1)
            (check-line line)
            (eof-object? line))
       (string-replace line "." ",")]
      [else line])))

(define func-list
  (list boomer-spacing
        improve-grammar))

(define (enhance-file file)
  (let ([line (read-line file 'any)])
    (unless (eof-object? line)
      (display
       ((foldl compose1 values func-list) (string-append line "\n")))
      (enhance-file file))))

(define (empress-begin [args (current-command-line-arguments)])
  (cond
    [(vector-empty? args)
     (error 'empress-begin "no file(s) given, arg list: ~a" args)]
    [else
     (for ([file args])
       (cond
         [(file-exists? file)
          (printf "File recieved for enhancement: ~a\n" file)
          (call-with-input-file file enhance-file)]
         [else
          (error 'empress-begin "no file named: ~a" file)]))]))

(empress-begin)
