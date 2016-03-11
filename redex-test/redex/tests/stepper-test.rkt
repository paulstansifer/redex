#lang racket/base

(require framework
         racket/class
         "private/test-util.rkt"
         redex/reduction-semantics
         redex/private/stepper
         redex/private/size-snip)

(module test racket/base)

(reset-count)

(define-language mt)

;; diff : term term -> (cons range range)
;; range = (listof (cons nat nat))
(define (diff from to)
  (define (make-node t)
    (new node%
         [pp default-pretty-printer]
         [all-nodes-ht 'dont-care]
         [term t]
         [red 'dont-care]
         [change-path 'dont-care]
         [init-cw (initial-char-width)]))
  (define (ranges node)
    (sort (map (λ (range) (cons (text:range-start range)
                                (text:range-end range)))
               (send (send (send node get-big-snip) get-editor)
                     get-highlighted-ranges))
          < #:key car))
  (define from-node (make-node from))
  (define to-node (make-node to))
  (show-diff from-node to-node)
  (cons (ranges from-node) (ranges to-node)))

(test (diff (term (hole a) #:lang mt) (term (hole b) #:lang mt))
      (cons (list (cons 6 7)) (list (cons 6 7))))
(test (diff (term (,'hole a) #:lang mt) (term (,'hole b) #:lang mt))
      (cons (list (cons 8 9)) (list (cons 8 9))))

(test (diff (term (() #f () #f) #:lang mt) (term (1 2 () #f) #:lang mt))
      (cons (list (cons 1 3) (cons 4 6))
            (list (cons 1 2) (cons 3 4))))
(test (diff (term (<> ((a b)) () e) #:lang mt) (term (<> ((a b)) () (c d)) #:lang mt))
      (cons (list (cons 15 16))
            (list (cons 15 20))))

(test (diff (term |a'| #:lang mt) (term b #:lang mt))
      (cons (list (cons 0 4)) (list (cons 0 1))))

(print-tests-passed 'stepper-test.rkt)
