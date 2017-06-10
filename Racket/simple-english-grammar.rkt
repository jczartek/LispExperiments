#lang racket
; This program generates random simple English sentences.

(define (sentence)
  (append (noun-phrase) (verb-phrase)))

(define (noun-phrase)
  (append (Article) (Adj*) (Noun) (PP*)))

(define (verb-phrase)
  (append (Verb) (noun-phrase)))

(define (Article)
  (one-of '(the a)))

(define (Noun)
  (one-of '(man ball woman table)))

(define (Verb)
  (one-of '(hit took saw liked)))

(define (PP)
  (append (Prep) (noun-phrase)))

(define (Adj)
  (one-of '(big little blue green adiabatic)))

(define (Prep)
  (one-of '(to in by with on)))

; Pick one element of set, and make a list of it.
(define (one-of set)
  (list (random-elt set)))

; Choose an element from a list at random.
(define (random-elt choices)
  (list-ref choices (random (length choices))))

(define (Adj*)
  (if (= (random 2) 0)
      null
      (append (Adj) (Adj*))))

(define (PP*)
  (if (random-elt '(#t #f))
      (append (PP) (PP*))
      null))
