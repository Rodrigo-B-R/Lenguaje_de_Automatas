#lang racket


(define (add-error errors esperado tok )
        (append errors
               (list (string-append "Error: esperaba " esperado
                                     ", se recibio " tok   ) )
        )
)

(define (add-automata automata estado tipo transicion)
  
  (cond
    ; establece el estado inicial
    [(equal? tipo 'inicial)
     (hash-set automata "iniciales" estado)]

    ; agrega un estado a la lista de finales
    [(equal? tipo 'final)
     (let ([finales-actuales (hash-ref automata "finales" '())])
       (hash-set automata "finales" (append finales-actuales (list estado))))]

    ; agrega una transicion: estado --simbolo--> siguiente
    [(equal? tipo 'transicion)
     (let* ([simbolo      (string->symbol (first transicion))]
            [siguiente    (second transicion)]
            [delta-estado (hash-ref automata estado (hash))])
       (hash-set automata estado (hash-set delta-estado simbolo siguiente)))]))

; ; statesPrime ::= coma stateId statesPrime | semicolon
(define (syn-statesPrime toks errors)
         (define ctok (caar toks) ) ;current token
         (cond [equal? ctok "coma"
            (define ntok (caadr toks))
            (cond [(equal? ntok "stateId")(
              (syn-statesPrime (cdr toks) errors))
          ]
            [else (add-error errors "coma" ntok)]
            )
         ]
         [(equal? ctok "semicol")
         (displayln "Done!")]
          
          
          
          )

)



; states  ::= stateId  statesPrime
(define (syn-states toks errors)
         (define ctok (caar toks) )
         (cond [(equal? ctok "stateId") (syn-statesPrime (cdr toks) errors )]
         [else (add-error errors "stateId" ctok)])
)


; statesDef ::=  kwStates  colon  states
(define (syn-statesDef toks errors)
       (define ctok (caar toks)) ; obtener el label del current token

       (cond
         [(equal? "kw-state" ctok)
          (define ntok (caadr toks))
          (cond
            [(equal? ntok "colon") (syn-states (cddr toks) errors)]
            [else (add-error errors "colon" ntok)])]

         [else (add-error errors "states" ctok)
         ; guarda el error
         ]
       )
)


; inicio del descenso 
(define (rec-des toks)
  (define errors '() )          ; lista para guardar errores
  (syn-statesDef toks errors )  ; parsea statesDef
)


; token stream (parcial): states : q0, q1, q2 ;
(define token-stream
              '(  ("kw-state" "states")
                  ("colon"    ":")
                  ("stateId"  "q0")
                  ("coma"     ",")
                  ("stateId"  "q1")
                  ("coma"     ",")
                  ("stateId"  "q2")
                  ("semicol"  ";")
                  ("EOF"      "EOF")
                ) )

; invoca el descenso recursivo
(rec-des token-stream)  