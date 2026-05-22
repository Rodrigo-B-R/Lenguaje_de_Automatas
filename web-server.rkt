#lang racket

(require web-server/servlet)
(require web-server/servlet-env)
(require json)
(require "lexer.rkt")
(require "parser.rkt")
(require "graph-renderer.rkt")
(require "validator.rkt")

(define (errors->html errors)
  (string-append
   "<!DOCTYPE html><html><body style='background:#1e1e1e;color:white;font-family:monospace;padding:20px'>"
   (apply string-append
          (map (lambda (e) (string-append "<p style='color:#f44747'>" e "</p>")) errors))
   "</body></html>"))

; Tokeniza y parsea el input. Devuelve (values html-resultado ok? automata-o-#f)
(define (process-input input)
  (define tokens (tokenize-all input))
  (define lex-error (error-tokens? tokens))
  (cond
    [lex-error
     (let-values ([(antes resto) (splitf-at tokens (lambda (t) (not (equal? (first t) "error"))))])
       (let* ([token-error   (first resto)]
              [lexema-error  (second token-error)]
              [linea-error   (if (>= (length token-error) 3) (number->string (third token-error)) "?")]
              [mensaje-error (list (list "error" (string-append " <- Error lexico: '" lexema-error "' en linea " linea-error)))])
         (values (tokens->html (append antes (list token-error) mensaje-error)) #f #f)))]
    [else
     (define parse-result  (rec-des (clean-token-stream tokens)))
     (define parse-errors  (first  parse-result))
     (define automata      (second parse-result))
     (if (null? parse-errors)
         (values (tokens->html tokens) #t automata)
         (values (errors->html parse-errors) #f #f))]))

(define (start request)
  (define method (request-method request))
  (cond
    [(equal? method #"POST")
     (let* ([data      (bytes->jsexpr (request-post-data/raw request))]
            [input-str (hash-ref data 'input)]
            [_cadena   (hash-ref data 'cadena "")])
       (let-values ([(resultado ok? automata) (process-input input-str)])
         (let ([json-hash (hash 'resultado resultado
                                'imagen    (if ok? (genera-imagen automata) "")
                                'valido    (and ok? (valida automata _cadena)))])
           (response/output #:code 200 #:mime-type #"application/json"
                            (lambda (out) (write-json json-hash out))))))]
    [else
     (response/output #:code 200 #:mime-type #"text/html"
                      (lambda (out) (display (file->string "index.html") out)))]))

(serve/servlet start
               #:launch-browser? #f
               #:servlet-path "/"
               #:listen-ip #f
               #:port (string->number (or (getenv "PORT") "8080")))
