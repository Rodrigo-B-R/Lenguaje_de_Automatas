#lang racket

;importa TOKENS de lexer
(require "lexer.rkt")

(define (add-error errors esperado tok num-linea)
  (append errors
          (list (string-append "Error: esperaba " esperado
                               ", se recibio " tok " en linea " num-linea))))

(define (resultado toks errors automata) (list toks errors automata))
(define (res-toks     r) (first  r))
(define (res-errors   r) (second r))
(define (res-automata r) (third  r))
(define (get-line-num toks)
  (let ([tok (car toks)])
    (if (>= (length tok) 3)
        (number->string (third tok))
        "?")))

(define (add-automata automata estado tipo transicion)
  (cond
    [(equal? tipo 'inicial)
     (hash-set automata "iniciales" estado)]
    [(equal? tipo 'final)
     (let ([finales-actuales (hash-ref automata "finales" '())])
       (hash-set automata "finales" (append finales-actuales (list estado))))]
    [(equal? tipo 'transicion)
     (let* ([simbolo      (string->symbol (first transicion))]
            [siguiente    (second transicion)]
            [delta-estado (hash-ref automata estado (hash))])
       (hash-set automata estado (hash-set delta-estado simbolo siguiente)))]))

; Recolecta lexemas de una lista: state|ID , state|ID ...
(define (collect-ids toks)
  (cond
    [(null? toks) '()]
    [(or (equal? (caar toks) TOKEN-ID)
         (equal? (caar toks) TOKEN-STATE)) (cons (cadar toks) (collect-ids (cdr toks)))]
    [(equal? (caar toks) TOKEN-COMA)       (collect-ids (cdr toks))]
    [else '()]))


; statesPrime ::= coma state statesPrime | epsilon
(define (syn-statesPrime toks errors automata)
  (define ctok (caar toks))
  (define num-linea (get-line-num toks))
  (cond
    [(equal? ctok TOKEN-COMA)
     (let ([ntok (caadr toks)])
       (cond
         [(equal? ntok TOKEN-STATE)
          (syn-statesPrime (cddr toks) errors automata)]
         [else
          (resultado toks (add-error errors TOKEN-STATE ntok num-linea) automata)]))]
    [else
     (resultado toks errors automata)]))

; states ::= state statesPrime
(define (syn-states toks errors automata)
  (define ctok (caar toks))
  (define num-linea (get-line-num toks))

  (cond
    [(equal? ctok TOKEN-STATE) (syn-statesPrime (cdr toks) errors automata)]
    [else (resultado toks (add-error errors TOKEN-STATE ctok num-linea) automata)]))


; symbolsPrime ::= coma ID symbolsPrime | epsilon
(define (syn-symbolsPrime toks errors automata)
  (define ctok (caar toks))
  (define num-linea (get-line-num toks))
  
  (cond
    [(equal? ctok TOKEN-COMA)
     (let ([ntok (caadr toks)])
       (cond
         [(equal? ntok TOKEN-ID)
          (syn-symbolsPrime (cddr toks) errors automata)]
         [else
          (resultado toks (add-error errors TOKEN-ID ntok num-linea) automata)]))]
    [else
     (resultado toks errors automata)]))

; symbols ::= ID symbolsPrime
(define (syn-symbols toks errors automata)
  (define ctok (caar toks))
  (define num-linea (get-line-num toks))


  (cond
    [(equal? ctok TOKEN-ID) (syn-symbolsPrime (cdr toks) errors automata)]
    [else (resultado toks (add-error errors TOKEN-ID ctok num-linea) automata)]))


; single-state ::= state
(define (syn-single-state toks errors automata)
  (define ctok (caar toks))
  (define num-linea (get-line-num toks))


  (cond
    [(equal? ctok TOKEN-STATE)
     (resultado (cdr toks) errors automata)]
    [else
     (resultado toks (add-error errors TOKEN-STATE ctok num-linea) automata)]))


; states: state , state ...
(define (syn-statesDef toks errors automata)
  (define ctok (caar toks))
  (define num-linea (get-line-num toks))


  (cond
    [(equal? ctok TOKEN-KW-STATE)
     (let ([ntok (caadr toks)])
       (cond
         [(equal? ntok TOKEN-COLON)
          (let* ([state-toks (cddr toks)]
                 [state-ids  (collect-ids state-toks)]
                 [automata*  (hash-set automata "states" state-ids)])
            (syn-states state-toks errors automata*))]
         [else
          (resultado toks (add-error errors TOKEN-COLON ntok num-linea) automata)]))]
    [else
     (resultado toks (add-error errors TOKEN-KW-STATE ctok num-linea) automata)]))


; start_state: state
(define (syn-stateStart toks errors automata)
  (define ctok (caar toks))
  (define num-linea (get-line-num toks))

  (cond
    [(equal? ctok TOKEN-START-STATE)
     (let ([ntok (caadr toks)])
       (cond
         [(equal? ntok TOKEN-COLON)
          (let* ([state-toks (cddr toks)]
                 [start-id   (and (pair? state-toks)
                                  (equal? (caar state-toks) TOKEN-STATE)
                                  (cadar state-toks))]
                 [automata*  (if start-id
                                 (add-automata automata start-id 'inicial #f)
                                 automata)])
            (syn-single-state state-toks errors automata*))]
         [else
          (resultado toks (add-error errors TOKEN-COLON ntok num-linea) automata)]))]
    [else
     (resultado toks (add-error errors TOKEN-START-STATE ctok num-linea) automata)]))


; accept_states: state , state ...
(define (syn-statesAccepted toks errors automata)
  (define ctok (caar toks))
  (define num-linea (get-line-num toks))

  (cond
    [(equal? ctok TOKEN-ACCEPT-STATES)
     (let ([ntok (caadr toks)])
       (cond
         [(equal? ntok TOKEN-COLON)
          (let* ([state-toks (cddr toks)]
                 [accept-ids (collect-ids state-toks)]
                 [automata*  (foldl (lambda (id acc) (add-automata acc id 'final #f))
                                    automata
                                    accept-ids)])
            (syn-states state-toks errors automata*))]
         [else
          (resultado toks (add-error errors TOKEN-COLON ntok num-linea) automata)]))]
    [else
     (resultado toks (add-error errors TOKEN-ACCEPT-STATES ctok num-linea) automata)]))


; input_alphabet: ID , ID ...
(define (syn-alphabet toks errors automata)
  (define ctok (caar toks))
  (define num-linea (get-line-num toks))

  (cond
    [(equal? ctok TOKEN-ALPHABET)
     (let ([ntok (caadr toks)])
       (cond
         [(equal? ntok TOKEN-COLON)
          (let* ([alpha-toks (cddr toks)]
                 [symbols    (collect-ids alpha-toks)]
                 [automata*  (hash-set automata "alphabet" symbols)])
            (syn-symbols alpha-toks errors automata*))]
         [else
          (resultado toks (add-error errors TOKEN-COLON ntok num-linea) automata)]))]
    [else
     (resultado toks (add-error errors TOKEN-ALPHABET ctok num-linea) automata)]))


; deltaPrime ::= state colon ID colon state deltaPrime | epsilon
(define (syn-deltaPrime toks errors automata)
  (define ctok (caar toks))
  (define num-linea (get-line-num toks))

  (cond
    [(not (equal? ctok TOKEN-STATE))
     (resultado toks errors automata)]
    [else
     (let ([t2 (caadr        toks)]
           [t3 (caaddr       toks)]
           [t4 (caar (cdddr  toks))]
           [t5 (caar (cddddr toks))])
       (cond
         [(not (equal? t2 TOKEN-COLON)) (resultado toks (add-error errors TOKEN-COLON t2 num-linea) automata)]
         [(not (equal? t3 TOKEN-ID))    (resultado toks (add-error errors TOKEN-ID    t3 num-linea) automata)]
         [(not (equal? t4 TOKEN-COLON)) (resultado toks (add-error errors TOKEN-COLON t4 num-linea) automata)]
         [(not (equal? t5 TOKEN-STATE)) (resultado toks (add-error errors TOKEN-STATE t5 num-linea) automata)]
         [else
          (let* ([from      (cadar         toks)]
                 [sym       (cadar (cddr   toks))]
                 [to        (cadar (cddddr toks))]
                 [automata* (add-automata automata from 'transicion (list sym to))])
            (syn-deltaPrime (cdr (cddddr toks)) errors automata*))]))]))

; deltafirst ::= state colon ID colon state deltaPrime
(define (syn-deltafirst toks errors automata)
  (define ctok (caar toks))
  (define num-linea (get-line-num toks))

  (cond
    [(equal? ctok TOKEN-STATE)
     (let ([t2 (caadr        toks)]
           [t3 (caaddr       toks)]
           [t4 (caar (cdddr  toks))]
           [t5 (caar (cddddr toks))])
       (cond
         [(not (equal? t2 TOKEN-COLON)) (resultado toks (add-error errors TOKEN-COLON t2 num-linea) automata)]
         [(not (equal? t3 TOKEN-ID))    (resultado toks (add-error errors TOKEN-ID    t3 num-linea) automata)]
         [(not (equal? t4 TOKEN-COLON)) (resultado toks (add-error errors TOKEN-COLON t4 num-linea) automata)]
         [(not (equal? t5 TOKEN-STATE)) (resultado toks (add-error errors TOKEN-STATE t5 num-linea) automata)]
         [else
          (let* ([from      (cadar         toks)]
                 [sym       (cadar (cddr   toks))]
                 [to        (cadar (cddddr toks))]
                 [automata* (add-automata automata from 'transicion (list sym to))])
            (syn-deltaPrime (cdr (cddddr toks)) errors automata*))]))]
    [else
     (resultado toks (add-error errors TOKEN-STATE ctok num-linea) automata)]))

; delta: deltafirst
(define (syn-delta toks errors automata)
  (define ctok (caar toks))
  (define num-linea (get-line-num toks))

  (cond
    [(equal? ctok TOKEN-DELTA)
     (let ([ntok (caadr toks)])
       (cond
         [(equal? ntok TOKEN-COLON)
          (syn-deltafirst (cddr toks) errors automata)]
         [else          
          (resultado toks (add-error errors TOKEN-COLON ntok num-linea) automata)]))]
    [else
     (resultado toks (add-error errors TOKEN-DELTA ctok num-linea) automata)]))


; Devuelve (list errores automata-hash)
(define (rec-des toks)
  (define init-automata (hash "finales" '() "iniciales" #f "states" '() "alphabet" '()))
  (define errors '())
  (define r1 (syn-statesDef      toks          errors init-automata))
  (define r2 (syn-stateStart     (res-toks r1) (res-errors r1) (res-automata r1)))
  (define r3 (syn-statesAccepted (res-toks r2) (res-errors r2) (res-automata r2)))
  (define r4 (syn-alphabet       (res-toks r3) (res-errors r3) (res-automata r3)))
  (define r5 (syn-delta          (res-toks r4) (res-errors r4) (res-automata r4)))

  (define tok-final
    (if (null? (res-toks r5))
        TOKEN-EOF
        (caar (res-toks r5))))

  (define final-errors
    (if (equal? tok-final TOKEN-EOF)
        (res-errors r5)
        (add-error (res-errors r5) TOKEN-EOF tok-final (get-line-num (res-toks r5)))))

  (list final-errors (res-automata r5)))

(provide rec-des)
