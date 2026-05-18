#lang racket

(require web-server/servlet)
(require web-server/servlet-env)
(require json)
(require "draw-graph-modulo.rkt")
(require "tokenizar.rkt")
(require "parser_descenso_recursivo.rkt")
(require "automaton-simulation.rkt")

; Tokeniza y parsea el input. Devuelve (values html-resultado automata-o-#f)
(define (process-input input)
  (define lines (string-split input "\n"))
  (define tokens (tokenize-all lines))
  (define errors (error-tokens? tokens))
  (cond
    [errors
     (let-values ([(antes resto) (splitf-at tokens (lambda (t) (not (equal? (first t) "error"))))])
       (let* ([token-error   (first resto)]
              [lexema-error  (second token-error)]
              [mensaje-error (list (list "error" (string-append " <- Error lexico: '" lexema-error "'")))])
         (values (tokens->html (append antes (list token-error) mensaje-error)) #f)))]
    [else
     (let ([parse-result (parse-automaton tokens)])
       (cond
         [(equal? (first parse-result) 'syntax-error)
          (values (error->html (second parse-result)) #f)]
         [else
          (values (tokens->html tokens) (second parse-result))]))]))

(define (error->html msg)
  (string-append
   "<!DOCTYPE html><html><body style='background:#1e1e1e;color:white;font-family:monospace;padding:20px'>"
   "<p style='color:#f44747'>" msg "</p>"
   "</body></html>"))

(define (start request)
  (define method (request-method request))
  (cond
    [(equal? method #"POST")
     (let* ([data      (bytes->jsexpr (request-post-data/raw request))]
            [input-str (hash-ref data 'input)]
            [cadena    (hash-ref data 'cadena "")])
       (let-values ([(resultado automata) (process-input input-str)])
         (let* ([imagen    (if automata (genera-imagen automata) "")]
                [valido    (if (and automata (not (equal? cadena "")))
                               (valida automata cadena)
                               'null)]
                [json-hash (hash 'resultado resultado
                                 'imagen    imagen
                                 'valido    valido)])
           (response/output #:code 200 #:mime-type #"application/json"
                            (lambda (out) (write-json json-hash out))))))]

    [else
     (response/output #:code 200 #:mime-type #"text/html"
                      (lambda (out) (display (file->string "index.html") out)))]))

(serve/servlet start #:launch-browser? #t #:servlet-path "/")
