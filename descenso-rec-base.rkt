#lang racket

(define TOKEN-COMA          "comma_op")
(define TOKEN-ID            "ID")
(define TOKEN-NEWLINE       "newline")
(define TOKEN-COLON         "colon_op")
(define TOKEN-START-STATE   "start_state_op")
(define TOKEN-ACCEPT-STATES "accept_states_op")
(define TOKEN-KW-STATE      "states")
(define TOKEN-ALPHABET      "input_alphabet")
(define TOKEN-COMMENT       "comment_op")
(define TOKEN-DELTA "delta_op")
(define TOKEN-EOF "EOF")



(define (add-error errors esperado tok)
        (append errors
               (list (string-append "Error: esperaba " esperado
                                     ", se recibio " tok))))

(define (resultado toks errors) (cons toks errors))
(define (res-toks r)   (car r))
(define (res-errors r) (cdr r))


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

;#
(define (syn-comment toks errors)
  (define ctok (caar toks))
  (cond
    [(equal? ctok TOKEN-COMMENT) (syn-commentPrime (cdr toks) errors)]
    [else (resultado toks errors)]))

;dsfbdfadkjsbfakjsdhfkad newline
(define (syn-commentPrime toks errors)
  (define ctok (caar toks))
  (cond
    [(not (equal? ctok TOKEN-NEWLINE)) (syn-commentPrime (cdr toks) errors)]
    [else (resultado (cdr toks) errors)]))


; ,ID, ID ... comentarios opcional newline 
; statesPrime ::= coma stateId statesPrime | newline comentarios opcional
(define (syn-statesPrime toks errors)
  (define ctok (caar toks))
  (cond
    [(equal? ctok TOKEN-COMA)
     (let ([ntok (caadr toks)])
       (cond
         [(equal? ntok TOKEN-ID)
          (syn-statesPrime (cddr toks) errors)]
        ;  [(equal? ntok TOKEN-NEWLINE)
        ;   ; coma colgante: consumir coma + newline para no propagar el error
        ;   (resultado (cddr toks) (add-error errors TOKEN-ID ntok))]
         [else
          (resultado toks (add-error errors TOKEN-ID ntok))]))]
    [(equal? ctok TOKEN-NEWLINE)
     (resultado (cdr toks) errors)]
    [(equal? ctok TOKEN-COMMENT)       ; comentario inline → termina la lista
     (let ([r (syn-commentPrime (cdr toks) errors)])
       (resultado (res-toks r) (res-errors r)))]
    [else
     (resultado toks (add-error errors TOKEN-COMA ctok))]))





; stateId newline :: opcional comment_op hbdsifjfdsdsifin
(define (syn-single-state toks errors)
  (define r0    (syn-comment toks errors))
  (define toks* (res-toks r0))
  (define ctok  (caar toks*))
  (cond
    [(equal? ctok TOKEN-ID)
     (let ([ntok (caadr toks*)])
       (cond
         [(equal? ntok TOKEN-NEWLINE)
          (resultado (cddr toks*) (res-errors r0))]
         [(equal? ntok TOKEN-COMMENT)        ; comentario inline
          (let ([comment-res (syn-comment (cdr toks*) (res-errors r0))])
            (resultado (res-toks comment-res) (res-errors comment-res)))]
         [else
          (resultado toks* (add-error (res-errors r0) TOKEN-NEWLINE ntok))]))]
    [else
     (resultado toks* (add-error (res-errors r0) TOKEN-ID ctok))]))



; start_state :
(define (syn-stateStart toks errors)
  (define r0    (syn-comment toks errors))
  (define toks* (res-toks r0))
  (define ctok  (caar toks*))
  (cond
    [(equal? ctok TOKEN-START-STATE)
     (let ([ntok (caadr toks*)])
       (cond
         [(equal? ntok TOKEN-COLON)
          (syn-single-state (cddr toks*) (res-errors r0))]
         [else
          (resultado toks* (add-error (res-errors r0) TOKEN-COLON ntok))]))]
    [else
     (resultado toks* (add-error (res-errors r0) TOKEN-START-STATE ctok))]))

; accept_states :
(define (syn-statesAccepted toks errors)
  (define r0    (syn-comment toks errors))
  (define toks* (res-toks r0))
  (define ctok  (caar toks*))
  (cond
    [(equal? ctok TOKEN-ACCEPT-STATES)
     (let ([ntok (caadr toks*)])
       (cond
         [(equal? ntok TOKEN-COLON)
          (syn-states (cddr toks*) (res-errors r0))]
         [else
          (resultado toks* (add-error (res-errors r0) TOKEN-COLON ntok))]))]
    [else
     (resultado toks* (add-error (res-errors r0) TOKEN-ACCEPT-STATES ctok))]))

;alphabet : 
(define (syn-alphabet toks errors)
  (define r0    (syn-comment toks errors))
  (define toks* (res-toks r0))
  (define ctok  (caar toks*))
  (cond
    [(equal? ctok TOKEN-ALPHABET)
     (let ([ntok (caadr toks*)])
       (cond
         [(equal? ntok TOKEN-COLON)
          (syn-states (cddr toks*) (res-errors r0))]
         [else
          (resultado toks* (add-error (res-errors r0) TOKEN-COLON ntok))]))]
    [else
     (resultado toks* (add-error (res-errors r0) TOKEN-ALPHABET ctok))]))

;stateId: stateIdPrime
; states  ::= stateId  statesPrime
(define (syn-states toks errors)
  (define ctok (caar toks))
  (cond
    [(equal? ctok TOKEN-ID) (syn-statesPrime (cdr toks) errors)]
    [else (resultado toks (add-error errors TOKEN-ID ctok))]))


;states:
; statesDef ::=  kwStates  colon  states
(define (syn-statesDef toks errors)
  (define r0    (syn-comment toks errors))
  (define toks* (res-toks r0))
  (define ctok  (caar toks*))
  (cond
    [(equal? ctok TOKEN-KW-STATE)
     (let ([ntok (caadr toks*)])
       (cond
         [(equal? ntok TOKEN-COLON) (syn-states (cddr toks*) (res-errors r0))]
         [else (resultado toks* (add-error (res-errors r0) TOKEN-COLON ntok))]))]
    [else (resultado toks* (add-error (res-errors r0) TOKEN-KW-STATE ctok))]))

; delta: newline
(define (syn-delta toks errors)
  (define r0    (syn-comment toks errors))
  (define toks* (res-toks r0))
  (define ctok  (caar toks*))
  (cond
    [(equal? ctok TOKEN-DELTA)
     (let ([ntok (caadr toks*)])
       (cond
         [(equal? ntok TOKEN-COLON)
          (let ([ntok2 (caaddr toks*)])
            (cond
              [(equal? ntok2 TOKEN-NEWLINE)
               (syn-deltafirst (cdddr toks*) (res-errors r0))]
              [(equal? ntok2 TOKEN-COMMENT)
               (let ([r (syn-comment (cddr toks*) (res-errors r0))])
                 (syn-deltafirst (res-toks r) (res-errors r)))]
              [else
               (resultado toks* (add-error (res-errors r0) TOKEN-NEWLINE ntok2))]))]
         [else
          (resultado toks* (add-error (res-errors r0) TOKEN-COLON ntok))]))]
    [else
     (resultado toks* (add-error (res-errors r0) TOKEN-DELTA ctok))]))

; id colon id colon id (comment?) newline syn-deltaPrime
(define (syn-deltafirst toks errors)
  (define r0    (syn-comment toks errors))
  (define toks* (res-toks r0))
  (define ctok  (caar toks*))
  (cond
    [(equal? ctok TOKEN-ID)
     (let ([t2 (caadr    toks*)]
           [t3 (caaddr   toks*)]
           [t4 (caar (cdddr  toks*))]
           [t5 (caar (cddddr toks*))])
       (cond
         [(not (equal? t2 TOKEN-COLON)) (resultado toks* (add-error (res-errors r0) TOKEN-COLON t2))]
         [(not (equal? t3 TOKEN-ID))    (resultado toks* (add-error (res-errors r0) TOKEN-ID    t3))]
         [(not (equal? t4 TOKEN-COLON)) (resultado toks* (add-error (res-errors r0) TOKEN-COLON t4))]
         [(not (equal? t5 TOKEN-ID))    (resultado toks* (add-error (res-errors r0) TOKEN-ID    t5))]
         [else
          (let* ([after5 (cdr (cddddr toks*))]
                 [next   (caar after5)])
            (cond
              [(equal? next TOKEN-NEWLINE)
               (syn-deltaPrime (cdr after5) (res-errors r0))]
              [(equal? next TOKEN-COMMENT)
               (let ([r (syn-comment after5 (res-errors r0))])
                 (syn-deltaPrime (res-toks r) (res-errors r)))]
              [else
               (resultado after5 (add-error (res-errors r0) TOKEN-NEWLINE next))]))]))]
    [else
     (resultado toks* (add-error (res-errors r0) TOKEN-ID ctok))]))


; syn-deltaPrime ::= id colon id colon id (comment?) newline syn-deltaPrime | epsilon
(define (syn-deltaPrime toks errors)
  (define ctok (caar toks))
  (cond
    [(not (equal? ctok TOKEN-ID))
     (resultado toks errors)]  ; epsilon: no hay mas transiciones
    [else
     (let ([t2 (caadr    toks)]
           [t3 (caaddr   toks)]
           [t4 (caar (cdddr  toks))]
           [t5 (caar (cddddr toks))])
       (cond
         [(not (equal? t2 TOKEN-COLON)) (resultado toks (add-error errors TOKEN-COLON t2))]
         [(not (equal? t3 TOKEN-ID))    (resultado toks (add-error errors TOKEN-ID    t3))]
         [(not (equal? t4 TOKEN-COLON)) (resultado toks (add-error errors TOKEN-COLON t4))]
         [(not (equal? t5 TOKEN-ID))    (resultado toks (add-error errors TOKEN-ID    t5))]
         [else
          (let* ([after5 (cdr (cddddr toks))]
                 [next   (caar after5)])
            (cond
              [(equal? next TOKEN-NEWLINE)
               (syn-deltaPrime (cdr after5) errors)]
              [(equal? next TOKEN-COMMENT)
               (let ([r (syn-comment after5 errors)])
                 (syn-deltaPrime (res-toks r) (res-errors r)))]
              [else
               (resultado after5 (add-error errors TOKEN-NEWLINE next))]))]))]))


  




; inicio del descenso
(define (rec-des toks)
  (define errors '())
  (define r1 (syn-statesDef      toks          errors))
  (define r2 (syn-stateStart     (res-toks r1) (res-errors r1)))
  (define r3 (syn-statesAccepted (res-toks r2) (res-errors r2)))
  (define r4 (syn-alphabet       (res-toks r3) (res-errors r3)))
  (define r5 (syn-delta          (res-toks r4) (res-errors r4)))

  (define tok-final
    (if (null? (res-toks r5))
        TOKEN-EOF
        (caar (res-toks r5))))

  (if (equal? tok-final TOKEN-EOF)
      (res-errors r5)
      (add-error (res-errors r5) TOKEN-EOF tok-final)))

(provide rec-des)
