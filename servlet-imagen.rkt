#lang racket

(require web-server/servlet)
(require web-server/servlet-env)
(require json)
(require "draw-graph-modulo.rkt")
(require "tokenizar.rkt")
(require "automaton-simulation.rkt")



(define (process-input input)
  (define lines (string-split input "\n"))

  (let* ([tokens (tokenize-all lines) ] [errors (error-tokens? tokens)] ) 
  (if errors
        (let-values ([(antes resto) (splitf-at tokens (lambda (t) (not (equal? (first t) "error" ))))])
          (let* ([token-error (first resto)]
                 [lexema-error (second token-error)]
                 [mensaje-error (list (list "error" (string-append " ← Error en: '" lexema-error "'")))])
                 (tokens->html (append antes (list token-error) mensaje-error))
                 )
            )
        (tokens->html tokens)  
        )))


; la funcion principal del servlet        archivo servlet-imagen.rkt
(define (start request)
  (define method (request-method request))  ; extrae el tipo de request (GET o POST)

  (cond
    [(equal? method #"POST")  ; cuando es un POST...

      (define data  (bytes->jsexpr  (request-post-data/raw request)) )  ; extrae el json
      (define input-str (hash-ref data 'input) )                        ; obtiene su campo input

      ; forma un hashmap con el texto de resultado y la imagen generada
      (define json-hash
              (hash 'resultado (process-input input-str )
                    'imagen    (genera-imagen '("la representacion del automata") )
              )
      )
      ; response/output envia la respuesta, escribe el hashmap hacia el json
      (response/output  #:code 200   #:mime-type #"application/json"
                    (lambda (out) (write-json json-hash out ) ) )
    ]

    ; si no es POST, asume es GET, y abre la pagina inical
    [else
          ; response/output pasa el puerto out a una funcion lambda,
          ; que 'imprime' lo que se mostrara
          (response/output  #:code 200   #:mime-type #"text/html"
                        (lambda (out) (display (file->string "index.html") out))  )
    ]
  )
)

(serve/servlet start #:launch-browser? #t #:servlet-path "/")