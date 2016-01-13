#lang racket/base

(require rackunit
         parser-tools/lex
         "alonzo.rkt")


;; hightlight-expr
(check-equal? (highlight-expr "(λx.x λx.x)" (highlight 6 4))
              (string-append "\n   (λx.x λx.x)"
                             "\n         ^^^^ \n\n"))
(check-equal? (highlight-expr "apple banana cauliflower" (list (highlight 6 1)
                                                               (highlight 13 1)))
              (string-append "\n   apple banana cauliflower"
                             "\n         ^      ^          \n\n"))


;; No input
(check-eq? (parse-λ-expr "") (void))


;; Good names
(check-equal? (parse-λ-expr "my-name") (name "my-name" (highlight 0 7)))
(check-equal? (parse-λ-expr "var") (name "var" (highlight 0 3)))


;; Bad names
(check-eq? (parse-λ-expr "-var") (void))
(check-eq? (parse-λ-expr "var-") (void))
(check-eq? (parse-λ-expr "some--var") (void))
(check-eq? (parse-λ-expr "&%#") (void))


;; Good functions
(check-equal? (parse-λ-expr "λx.x") (function "x" (name "x" (highlight 3 1)) (highlight 0 4)))
(check-equal? (parse-λ-expr "λa.b") (function "a" (name "b" (highlight 3 1)) (highlight 0 4)))
(check-equal? (parse-λ-expr "λthis-is-ok.this-is-ok") (function "this-is-ok" (name "this-is-ok" (highlight 12 10)) (highlight 0 22)))
(check-equal? (parse-λ-expr "λo-k.o-k-too") (function "o-k" (name "o-k-too" (highlight 5 7)) (highlight 0 12)))
(check-equal? (parse-λ-expr "λfirst.λsecond.first")
              (function "first" (function "second" (name "first" (highlight 15 5))
                                          (highlight 7 13))
                        (highlight 0 20)))


;; Bad functions
(check-eq? (parse-λ-expr "λ") (void))
(check-eq? (parse-λ-expr "λx") (void))
(check-eq? (parse-λ-expr "x.x") (void))
(check-eq? (parse-λ-expr "λx.x.x") (void))


;; Good applications
(check-equal? (parse-λ-expr "(x x)") (application (name "x" (highlight 1 1)) (name "x" (highlight 3 1)) (highlight 0 5)))
(check-equal? (parse-λ-expr "(a b)") (application (name "a" (highlight 1 1)) (name "b" (highlight 3 1)) (highlight 0 5)))
(check-equal? (parse-λ-expr "(func-tion arg-ument)") (application (name "func-tion" (highlight 1 9))
                                                                  (name "arg-ument" (highlight 11 9))
                                                                  (highlight 0 21)))
(check-equal? (parse-λ-expr "((a b) (c d))") (application (application (name "a" (highlight 2 1))
                                                                       (name "b" (highlight 4 1))
                                                                       (highlight 1 5))
                                                          (application (name "c" (highlight 8 1))
                                                                       (name "d" (highlight 10 1))
                                                                       (highlight 7 5))
                                                          (highlight 0 13)))


;; Bad applications
(check-eq? (parse-λ-expr "(a)") (void))
(check-eq? (parse-λ-expr "(a b c)") (void))
(check-eq? (parse-λ-expr "((a b)") (void))
(check-eq? (parse-λ-expr "(a b))") (void))


;; Good compound expressions
(check-equal? (parse-λ-expr "(λx.x λx.x)")
              (application (function "x" (name "x" (highlight 4 1)) (highlight 1 4))
                           (function "x" (name "x" (highlight 9 1)) (highlight 6 4))
                           (highlight 0 11)))
(check-equal? (parse-λ-expr "λs.(s s)")
              (function "s" (application (name "s" (highlight 4 1)) (name "s" (highlight 6 1))
                                         (highlight 3 5))
                        (highlight 0 8)))
(check-equal? (parse-λ-expr "(λx.x doesnt-matter)")
              (application (function "x" (name "x" (highlight 4 1)) (highlight 1 4))
                           (name "doesnt-matter" (highlight 6 13))
                           (highlight 0 20)))
(check-equal? (parse-λ-expr "λf.λa.(f a)")
              (function "f" (function "a" (application (name "f" (highlight 7 1))
                                                       (name "a" (highlight 9 1))
                                                       (highlight 6 5))
                                      (highlight 3 8))
                        (highlight 0 11)))
(check-equal? (parse-λ-expr "(λx.x λa.λb.b)")
              (application (function "x" (name "x" (highlight 4 1)) (highlight 1 4))
                           (function "a" (function "b" (name "b" (highlight 12 1)) (highlight 9 4))
                                     (highlight 6 7))
                           (highlight 0 14)))
(check-equal? (parse-λ-expr "((λfirst.λsecond.second λx.x) λfunc.λarg.(func arg))")
              (application (application (function "first" (function "second" (name "second" (highlight 17 6))
                                                                    (highlight 9 14))
                                                  (highlight 2 21))                                    
                                        (function "x" (name "x" (highlight 27 1))
                                                  (highlight 24 4))
                                        (highlight 1 28))
                           (function "func" (function "arg" (application (name "func" (highlight 42 4))
                                                                         (name "arg" (highlight 47 3))
                                                                         (highlight 41 10))
                                                      (highlight 36 15))
                                     (highlight 30 21))
                           (highlight 0 52)))


;; Correctly identify free variables
(check-equal? (free "name") (list (name "name" (highlight 0 4))))
(check-equal? (free "λx.x") '())
(check-equal? (free "(λx.x y)") (list (name "y" (highlight 6 1))))
(check-equal? (free "(a b)") (list (name "a" (highlight 1 1))
                                   (name "b" (highlight 3 1))))
(check-equal? (free "λy.(λx.y λy.x)") (list (name "x" (highlight 12 1))))
(check-equal? (free "(λx.y λy.x)") (list (name "y" (highlight 4 1))
                                         (name "x" (highlight 9 1))))
(check-equal? (free "λa.(λb.a λb.(λa.a b))") '())
(check-equal? (free "(λb.a λb.(λa.a b))") (list (name "a" (highlight 4 1))))


;; Correctly identify bound variables
(check-equal? (bound "name") '())
(check-equal? (bound "λx.x") (list (name "x" (highlight 3 1))))
(check-equal? (bound "(λx.x y)") (list (name "x" (highlight 4 1))))
(check-equal? (bound "(a b)") '())
(check-equal? (bound "λy.(λx.y λy.x)") (list (name "y" (highlight 7 1))))
(check-equal? (bound "(λx.y λy.x)") '())
(check-equal? (bound "λa.(λb.a λb.(λa.a b))") (list (name "a" (highlight 7 1))
                                                    (name "a" (highlight 16 1))
                                                    (name "b" (highlight 18 1))))
(check-equal? (bound "(λb.a λb.(λa.a b))") (list (name "a" (highlight 13 1))
                                                 (name "b" (highlight 15 1))))


;; Names should eval to themselves
(check-equal? (eval "x") (name "x" (highlight 0 1)))
(check-equal? (eval "another-name") (name "another-name" (highlight 0 12)))

;; Functions should eval to themselves
(check-equal? (eval "λx.x")
              (function "x" (name "x" #f) (highlight 0 4)))
(check-equal? (eval "λs.(s s)")
              (function "s" (application (name "s" #f)
                                         (name "s" #f)
                                         (highlight 3 5))
                        (highlight 0 8)))

;; Applications should apply properly
(check-equal? (eval "(λx.x a)")
              (name "a" (highlight 6 1)))
(check-equal? (eval "((λx.x λy.y) a)")
              (name "a" (highlight 13 1)))
(check-equal? (eval "((λx.x λx.x) a)")
              (name "a" (highlight 13 1)))
(check-equal? (eval "(λa.a (λx.x b))")
              (name "b" (highlight 12 1)))
(check-equal? (eval "((λa.a λb.b) (λc.c λd.d))")
              (function "d" (name "d" #f) (highlight 19 4)))
(check-equal? (eval "(λx.x λs.(s s))")
              (function "s" (application (name "s" #f) (name "s" #f)
                                         (highlight 9 5))
                        (highlight 6 8)))
(check-equal? (eval "(λf.λa.(f a) n)")
              (function "a" (application (name "n" (highlight 13 1))
                                         (name "a" #f)
                                         (highlight 7 5))
                        (highlight 4 8)))

(check-equal? (eval "(λf.λa.λf.(f a) n)")
              (function "a" (function "f" (application (name "f" #f)
                                                       (name "a" #f)
                                                       (highlight 10 5))
                                      (highlight 7 8))
                        (highlight 4 11)))

(check-equal? (eval "(λf.λa.λf.λz.(f a) n)")
              (function "a" (function "f" (function "z" (application (name "f" #f)
                                                                     (name "a" #f)
                                                                     (highlight 13 5))
                                                    (highlight 10 8))
                                      (highlight 7 11))
                        (highlight 4 14)))


