#lang racket/base

(require rackunit
         parser-tools/lex
         "alonzo.rkt")

(check-equal? (highlight-expr "(λx.x λx.x)" (srcloc #f #f #f 7 4))
              (string-append "\n   (λx.x λx.x)"
                             "\n         ^^^^ \n\n"))
(check-equal? (highlight-expr "apple banana cauliflower" (list (srcloc #f #f #f 7 1)
                                                               (srcloc #f #f #f 14 1)))
              (string-append "\n   apple banana cauliflower"
                             "\n         ^      ^          \n\n"))

;(check-exn exn:fail? (λ () (parse-λ-expr "")))

(check-equal? (parse-λ-expr "my-name") (name "my-name"))
(check-equal? (parse-λ-expr "var") (name "var"))
;(check-equal? (parse-λ-expr "-var") (name "-var"))
;(check-equal? (parse-λ-expr "var-") (name "var-"))
;(check-equal? (parse-λ-expr "some--var") (name "some--var"))


(check-equal? (parse-λ-expr "λx.x") (function "x" (name "x")))
(check-equal? (parse-λ-expr "(a b)") (application (name "a") (name "b")))

              
