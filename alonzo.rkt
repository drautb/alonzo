#lang racket/base

(require racket/list
         racket/string
         parser-tools/lex
         parser-tools/yacc
         (prefix-in sre- parser-tools/lex-sre))

(provide (all-defined-out))

;; highlight-expr -> string? -> (or srcloc? (listof? srcloc))
;; Displays expr-str which each srcloc highlighted based on position and span
(define (highlight-expr expr-str srclocs)
  (define highlights (if (list? srclocs) srclocs (list srclocs)))
  (define highlight-str (make-string (string-length expr-str) #\space))
  (for-each (λ (loc)
              (for ([n (srcloc-span loc)])
                (string-set! highlight-str
                             (+ n (sub1 (srcloc-position loc)))
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


;; Lanugage constructs
(struct name (name-expr) #:prefab)
(struct function (name-expr body-expr) #:prefab)
(struct application (function-expr argument-expr) #:prefab)

;; Parser definitions
(define λ-expr-parser
  (parser
   (start expr)
   (end EOF)
   (src-pos)
   (error (λ (tok-ok? tok-name tok-value start-pos end-pos)
            (printf "TOK-OK? ~a~nTOK-NAME: ~a~nTOK-VALUE: ~a~nSTART: ~a~nEND: ~a~n" tok-ok? tok-name tok-value start-pos end-pos)))
   (tokens λ-expr-fixed-tokens λ-expr-tokens)
   ;(precs ... ?)
   (grammar
    (expr ((NAME) (name $1))
          ((LAMBDA NAME PERIOD expr) (function $2 $4))
          ((BEGIN-APPLICATION expr expr END-APPLICATION) (application $2 $3))))))

(define (parse-λ-expr expr)
  (define (handle-errors e)
    (define (handle-lexer-exn e)
      (let ([loc (srcloc-position (first (exn:fail:read-srclocs e)))])
        (printf "Alonzo couldn't understand that expression, there was an unexpected character at position ~a.~n" loc)
        (printf (highlight-expr expr (exn:fail:read-srclocs e)))))
    (define (handle-parser-exn e)
      (void))
    (if (string-prefix? (exn-message e) "lexer")
        (handle-lexer-exn e)
        (handle-parser-exn e)))
  (let ([input (open-input-string expr)])
    (with-handlers ([exn:fail:read? handle-errors])
      (λ-expr-parser (λ () (λ-lexer input))))))

