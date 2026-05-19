#lang racket

(define TOKEN-COMA          "coma")
(define TOKEN-ID            "ID")
(define TOKEN-NEWLINE       "newline")
(define TOKEN-COLON         "colon")
(define TOKEN-START-STATE   "start_state_op")
(define TOKEN-ACCEPT-STATES "accept_states_op")
(define TOKEN-KW-STATE      "kw-state")


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


; ,q0, q1, q2 ... newline
; statesPrime ::= coma stateId statesPrime | newline
(define (syn-statesPrime toks errors)
  (define ctok (caar toks))
  (cond
    [(equal? ctok TOKEN-COMA)
     (let ([ntok (caadr toks)])
       (cond
         [(equal? ntok TOKEN-ID)
          (syn-statesPrime (cddr toks) errors)]
         [else
          (add-error errors TOKEN-ID ntok)]))]
    [(equal? ctok TOKEN-NEWLINE)
     (cdr toks)] ;devolver tokens sin parsear 
    [else
     (add-error errors TOKEN-COMA ctok)]))


; stateId newline
(define (syn-single-state toks errors)
  (define ctok (caar toks))
  (cond
    [(equal? ctok TOKEN-ID)
     (let ([ntok (caadr toks)])
       (cond
         [(equal? ntok TOKEN-NEWLINE)
          (cddr toks)]
         [else
          (add-error errors TOKEN-NEWLINE ntok)]))]
    [else
     (add-error errors TOKEN-ID ctok)]))

; start_state :
(define (syn-stateStart toks errors)
  (define ctok (caar toks))
  (cond
    [(equal? ctok TOKEN-START-STATE)
     (let ([ntok (caadr toks)])
       (cond
         [(equal? ntok TOKEN-COLON)
          (syn-single-state (cddr toks) errors)]
         [else
          (add-error errors TOKEN-COLON ntok)]))]
    [else
     (add-error errors TOKEN-START-STATE ctok)]))

; accept_states :
(define (syn-statesAccepted toks errors)

  (define ctok (caar toks)) ;current token

  (cond
    [(equal? ctok TOKEN-ACCEPT-STATES)
      (define ntok (caadr toks))
      (cond [(equal? ntok TOKEN-COLON)
        (syn-states (cddr toks) errors)
    ]
  [else (add-error errors TOKEN-COLON ntok)])

]
[else (add-error errors TOKEN-ACCEPT-STATES ctok)])
)

(define (syn-alphabet toks errors)
  (define ctok (caar toks)) ;current token
  (cond
    [(equal? ctok TOKEN-ID)
     (let ([ntok (caadr toks)])
       (cond
         [(equal? ntok TOKEN-NEWLINE)
          (cddr toks)]
         [else
          (add-error errors TOKEN-NEWLINE ntok)]))]
    [else
     (add-error errors TOKEN-ID ctok)]))






; states  ::= stateId  statesPrime
(define (syn-states toks errors)
  (define ctok (caar toks))
  (cond
    [(equal? ctok TOKEN-ID) (syn-statesPrime (cdr toks) errors)]
    [else (add-error errors TOKEN-ID ctok)]))


;states:
; statesDef ::=  kwStates  colon  states
(define (syn-statesDef toks errors)
  (define ctok (caar toks))
  (cond
    [(equal? ctok TOKEN-KW-STATE)
     (let ([ntok (caadr toks)])
       (cond
         [(equal? ntok TOKEN-COLON) (syn-states (cddr toks) errors)]
         [else (add-error errors TOKEN-COLON ntok)]))]
    [else (add-error errors TOKEN-KW-STATE ctok)]))


; inicio del descenso
(define (rec-des toks)
  (define errors '() )          ; lista para guardar errores
  (define r1 (syn-statesDef toks errors ) ) ; parsea statesDef
  (define r2 (syn-stateStart r1 errors))
  (define r3 (syn-statesAccepted r2 errors) )
  (define r4 (syn-alphabet r3 errors))
  
  r4
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
                  ("newline"      "newline")
                ) )

; invoca el descenso recursivo
(rec-des token-stream)
