#!/usr/bin/env racket

#lang racket/base

(require racket/list
         racket/match
         racket/string
         parser-tools/lex
         parser-tools/yacc
         (prefix-in sre- parser-tools/lex-sre))

(provide (struct-out highlight)
         (struct-out name)
         (struct-out function)
         (struct-out application)
         highlight-expr
         parse-λ-expr
         free
         bound
         eval)


;; Global variable for the current expression. This is bad, but
;; I haven't yet figured out another way to expose the original
;; expression to the parser's error function.
(define current-expr (void))


;; Structs
(struct highlight (start span) #:prefab)
(struct name (name highlight) #:prefab)
(struct function (name-expr body-expr highlight) #:prefab)
(struct application (function-expr argument-expr highlight) #:prefab)

;; srcloc->highlight -> srcloc? -> highlight?
;; Converts a srcloc? to a highlight?
(define (srcloc->highlight s)
  (highlight (srcloc-column s) (srcloc-span s)))


;; highlight-expr -> string? -> (or highlight? (listof? highlight)) -> None
;; Displays expr-str with each highlight displayed based on position and span
(define (highlight-expr expr-str highlights)
  (set! highlights (if (list? highlights) highlights (list highlights)))
  (define highlight-str (make-string (string-length expr-str) #\space))
  (for-each (λ (loc)
              (for ([n (highlight-span loc)])
                (string-set! highlight-str
                             (+ n (highlight-start loc))
                             #\^)))
            highlights)
  (format "~n   ~a~n   ~a~n~n" expr-str highlight-str))


;; Token definitions
(define-empty-tokens λ-expr-fixed-tokens
  (BEGIN-APPLICATION
    END-APPLICATION 
    LAMBDA
    PERIOD
    EOF))

(define-tokens λ-expr-tokens
  (NAME
   INVALID))


;; Lexer
(define-lex-abbrev
  ; Regex: [a-z]+(-[a-z]+)*
  valid-name (sre-= 1 (sre-seq (sre-+ (char-range #\a #\z))
                          (sre-* (sre-seq #\- (sre-+ (char-range #\a #\z)))))))


(define λ-lexer
  (lexer-src-pos
   [(eof) (token-EOF)]
   [whitespace (return-without-pos (λ-lexer input-port))]
   [#\( (token-BEGIN-APPLICATION)]
   [#\) (token-END-APPLICATION)]
   [#\λ (token-LAMBDA)]
   [#\. (token-PERIOD)]
   [valid-name (token-NAME lexeme)]))


;; Parser definitions
(define (handle-parser-error tok-ok? tok-name tok-value start-pos end-pos)
  (cond [(eq? tok-name 'EOF) (printf "Unexpected end of expression. (Did you forget a parenthesis?)~n")]
        [else (begin (printf "Unexpected ~a token~a~n" tok-name (if tok-value
                                                                    (format ": \"~a\"" tok-value)
                                                                     "."))
                     (printf (highlight-expr current-expr (highlight (position-col start-pos)
                                                                     (if tok-value (string-length tok-value) 1)))))]))


(define λ-expr-parser
  (parser
   (start expr)
   (end EOF)
   (src-pos)
   (error handle-parser-error)
   (tokens λ-expr-fixed-tokens λ-expr-tokens)
   ;(precs ... ?)
   ;(debug "/Users/drautb/GitHub/drautb/alonzo/LALR.txt")
   (grammar
    (expr ((NAME)
           (name $1 (highlight (position-col $1-start-pos)
                               (- (position-col $1-end-pos) (position-col $1-start-pos)))))
          ((LAMBDA NAME PERIOD expr)
           (function $2 $4
                     (highlight (position-col $1-start-pos)
                                (- (position-col $4-end-pos) (position-col $1-start-pos)))))
          ((BEGIN-APPLICATION expr expr END-APPLICATION)
           (application $2 $3
                        (highlight (position-col $1-start-pos)
                                   (- (position-col $4-end-pos) (position-col $1-start-pos)))))))))


;; parse-λ-expr -> string? -> (or/c name? function? application? void?)
;; Attempts to parse a string into a λ-expression. Returns void if errors exist.
(define (parse-λ-expr expr)
  (set! current-expr expr)
  (define (handle-errors e)
    (define (handle-lexer-exn e)
      (let ([loc (srcloc-position (first (exn:fail:read-srclocs e)))])
        (printf "Unrecognized character at position ~a.~n" loc)
        (printf (highlight-expr expr (map srcloc->highlight (exn:fail:read-srclocs e))))))
    (define (handle-parser-exn e)
      ;      (printf "'~a' is not a valid λ calculus expression.~n" expr))
      (void))
    (if (string-prefix? (exn-message e) "lexer")
        (handle-lexer-exn e)
        (handle-parser-exn e)))
  (let ([input (open-input-string expr)])
    (port-count-lines! input)
    (with-handlers ([exn:fail:read? handle-errors])
      (λ-expr-parser (λ () (λ-lexer input))))))


;; var-rec -> (or/c name? function? application?) -> (listof string?) -> procedure? -> (listof name?)
;; Takes a parsed λ-expression and returns a list of name? that satisfy procedure?
(define (var-rec parsed-expr env condition)
  (match parsed-expr
    [(name v h) (if (condition v env)
                    (cons (name v h) empty)
                    empty)]
    [(function n b h) (var-rec b (cons n env) condition)]
    [(application f a h) (cons (var-rec f env condition)
                               (cons (var-rec a env condition) empty))]))


;; free -> String -> (listof? name?)
;; Given a λ expression, get a list of all free variables in the expression.
(define (free expr)
  (define (free-in-env val env)
    (not (member val env)))
  (flatten (var-rec (parse-λ-expr expr) '() free-in-env)))


;; bound -> string? -> (listof? name?)
;; Given a λ expression, get a list of all bound variables in the expression.
(define (bound expr)
  (define (bound-in-env val env)
    (member val env))
  (flatten (var-rec (parse-λ-expr expr) '() bound-in-env)))


;; eval -> string? -> result?
;; Evaluates a λ expression, returning a result object.
;; Performs β-reduction, η-reduction,
(define (eval expr)
  (define parsed-expr (parse-λ-expr expr))
  (define (substitute-free-vars expr env)
    (match expr
      [(name v h)
       (let ([val (assoc v env)])
         (if val
             (cdr val)
             (name v h)))]
      [(function n b h)
       (function n (substitute-free-vars b (cons (cons n (name n #f)) env)) h)]
      [(application f a h)
       (application (substitute-free-vars f env)
                    (substitute-free-vars a env) h)]))
  (define (eval-rec parsed-expr env)
    (match parsed-expr
      ;; η-reduction pattern
      ;; λ<name>.(<expr> <name>) => <expr>
;      [(function n (application f (name n h1) h2) h3) f]

      ;; <name> should be replaced with its value from env, if one exists
      [(name v h) (let ([val (assoc v env)])
                    (if val
                        (cdr val)
                        (name v h)))]
                        
      ;; <function> should not be eval'd, but it should replace its free variables with
      ;; values from the environment.
      [(function n b h) (function n (substitute-free-vars b
                                                          (cons (cons n (name n #f)) env)) h)]
        
      ;; <application> should only work if <f> is a function.
      ;; Eval's <b> with (eval a) bound to <n>
      [(application f a h)
       (let ([func-expr (eval-rec f env)]
             [arg-expr (eval-rec a env)])
         (eval-rec (function-body-expr func-expr)
                   (cons (cons (function-name-expr func-expr) arg-expr)  env)))]
      ))
  (eval-rec parsed-expr '()))
