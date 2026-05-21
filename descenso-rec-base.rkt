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
(define TOKEN-DELTA         "delta_op")
(define TOKEN-EOF           "EOF")


(define (add-error errors esperado tok)
  (append errors
          (list (string-append "Error: esperaba " esperado
                               ", se recibio " tok))))

; resultado ahora carga (toks errors automata)
(define (resultado toks errors automata) (list toks errors automata))
(define (res-toks     r) (first  r))
(define (res-errors   r) (second r))
(define (res-automata r) (third  r))


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

; Extrae los lexemas de IDs de una secuencia ID , ID , ... (sin consumir tokens)
(define (collect-ids toks)
  (cond
    [(null? toks) '()]
    [(equal? (caar toks) TOKEN-ID)   (cons (cadar toks) (collect-ids (cdr toks)))]
    [(equal? (caar toks) TOKEN-COMA) (collect-ids (cdr toks))]
    [else '()]))


(define (syn-comment toks errors automata)
  (define ctok (caar toks))
  (cond
    [(equal? ctok TOKEN-COMMENT) (syn-commentPrime (cdr toks) errors automata)]
    [else (resultado toks errors automata)]))

(define (syn-commentPrime toks errors automata)
  (define ctok (caar toks))
  (cond
    [(not (equal? ctok TOKEN-NEWLINE)) (syn-commentPrime (cdr toks) errors automata)]
    [else (resultado (cdr toks) errors automata)]))


(define (syn-statesPrime toks errors automata)
  (define ctok (caar toks))
  (cond
    [(equal? ctok TOKEN-COMA)
     (let ([ntok (caadr toks)])
       (cond
         [(equal? ntok TOKEN-ID)
          (syn-statesPrime (cddr toks) errors automata)]
         [else
          (resultado toks (add-error errors TOKEN-ID ntok) automata)]))]
    [(equal? ctok TOKEN-NEWLINE)
     (resultado (cdr toks) errors automata)]
    [(equal? ctok TOKEN-COMMENT)
     (let ([r (syn-commentPrime (cdr toks) errors automata)])
       (resultado (res-toks r) (res-errors r) (res-automata r)))]
    [else
     (resultado toks (add-error errors TOKEN-COMA ctok) automata)]))


(define (syn-single-state toks errors automata)
  (define r0    (syn-comment toks errors automata))
  (define toks* (res-toks r0))
  (define ctok  (caar toks*))
  (cond
    [(equal? ctok TOKEN-ID)
     (let ([ntok (caadr toks*)])
       (cond
         [(equal? ntok TOKEN-NEWLINE)
          (resultado (cddr toks*) (res-errors r0) (res-automata r0))]
         [(equal? ntok TOKEN-COMMENT)
          (let ([r (syn-comment (cdr toks*) (res-errors r0) (res-automata r0))])
            (resultado (res-toks r) (res-errors r) (res-automata r)))]
         [else
          (resultado toks* (add-error (res-errors r0) TOKEN-NEWLINE ntok) (res-automata r0))]))]
    [else
     (resultado toks* (add-error (res-errors r0) TOKEN-ID ctok) (res-automata r0))]))


(define (syn-stateStart toks errors automata)
  (define r0    (syn-comment toks errors automata))
  (define toks* (res-toks r0))
  (define ctok  (caar toks*))
  (cond
    [(equal? ctok TOKEN-START-STATE)
     (let ([ntok (caadr toks*)])
       (cond
         [(equal? ntok TOKEN-COLON)
          (let* ([state-toks (cddr toks*)]
                 [start-id   (and (not (null? state-toks))
                                  (equal? (caar state-toks) TOKEN-ID)
                                  (cadar state-toks))]
                 [automata*  (if start-id
                                 (add-automata (res-automata r0) start-id 'inicial #f)
                                 (res-automata r0))])
            (syn-single-state state-toks (res-errors r0) automata*))]
         [else
          (resultado toks* (add-error (res-errors r0) TOKEN-COLON ntok) (res-automata r0))]))]
    [else
     (resultado toks* (add-error (res-errors r0) TOKEN-START-STATE ctok) (res-automata r0))]))


(define (syn-statesAccepted toks errors automata)
  (define r0    (syn-comment toks errors automata))
  (define toks* (res-toks r0))
  (define ctok  (caar toks*))
  (cond
    [(equal? ctok TOKEN-ACCEPT-STATES)
     (let ([ntok (caadr toks*)])
       (cond
         [(equal? ntok TOKEN-COLON)
          (let* ([state-toks (cddr toks*)]
                 [accept-ids (collect-ids state-toks)]
                 [automata*  (foldl (lambda (id acc) (add-automata acc id 'final #f))
                                    (res-automata r0)
                                    accept-ids)])
            (syn-states state-toks (res-errors r0) automata*))]
         [else
          (resultado toks* (add-error (res-errors r0) TOKEN-COLON ntok) (res-automata r0))]))]
    [else
     (resultado toks* (add-error (res-errors r0) TOKEN-ACCEPT-STATES ctok) (res-automata r0))]))


(define (syn-alphabet toks errors automata)
  (define r0    (syn-comment toks errors automata))
  (define toks* (res-toks r0))
  (define ctok  (caar toks*))
  (cond
    [(equal? ctok TOKEN-ALPHABET)
     (let ([ntok (caadr toks*)])
       (cond
         [(equal? ntok TOKEN-COLON)
          (let* ([alpha-toks (cddr toks*)]
                 [symbols    (collect-ids alpha-toks)]
                 [automata*  (hash-set (res-automata r0) "alphabet" symbols)])
            (syn-states alpha-toks (res-errors r0) automata*))]
         [else
          (resultado toks* (add-error (res-errors r0) TOKEN-COLON ntok) (res-automata r0))]))]
    [else
     (resultado toks* (add-error (res-errors r0) TOKEN-ALPHABET ctok) (res-automata r0))]))


(define (syn-states toks errors automata)
  (define ctok (caar toks))
  (cond
    [(equal? ctok TOKEN-ID) (syn-statesPrime (cdr toks) errors automata)]
    [else (resultado toks (add-error errors TOKEN-ID ctok) automata)]))


(define (syn-statesDef toks errors automata)
  (define r0    (syn-comment toks errors automata))
  (define toks* (res-toks r0))
  (define ctok  (caar toks*))
  (cond
    [(equal? ctok TOKEN-KW-STATE)
     (let ([ntok (caadr toks*)])
       (cond
         [(equal? ntok TOKEN-COLON)
          (let* ([state-toks (cddr toks*)]
                 [state-ids  (collect-ids state-toks)]
                 [automata*  (hash-set (res-automata r0) "states" state-ids)])
            (syn-states state-toks (res-errors r0) automata*))]
         [else
          (resultado toks* (add-error (res-errors r0) TOKEN-COLON ntok) (res-automata r0))]))]
    [else
     (resultado toks* (add-error (res-errors r0) TOKEN-KW-STATE ctok) (res-automata r0))]))


(define (syn-delta toks errors automata)
  (define r0    (syn-comment toks errors automata))
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
               (syn-deltafirst (cdddr toks*) (res-errors r0) (res-automata r0))]
              [(equal? ntok2 TOKEN-COMMENT)
               (let ([r (syn-comment (cddr toks*) (res-errors r0) (res-automata r0))])
                 (syn-deltafirst (res-toks r) (res-errors r) (res-automata r)))]
              [else
               (resultado toks* (add-error (res-errors r0) TOKEN-NEWLINE ntok2) (res-automata r0))]))]
         [else
          (resultado toks* (add-error (res-errors r0) TOKEN-COLON ntok) (res-automata r0))]))]
    [else
     (resultado toks* (add-error (res-errors r0) TOKEN-DELTA ctok) (res-automata r0))]))


; from: sym: to
(define (syn-deltafirst toks errors automata)
  (define r0    (syn-comment toks errors automata))
  (define toks* (res-toks r0))
  (define ctok  (caar toks*))
  (cond
    [(equal? ctok TOKEN-ID)
     (let ([t2 (caadr           toks*)]
           [t3 (caaddr          toks*)]
           [t4 (caar (cdddr     toks*))]
           [t5 (caar (cddddr    toks*))])
       (cond
         [(not (equal? t2 TOKEN-COLON)) (resultado toks* (add-error (res-errors r0) TOKEN-COLON t2) (res-automata r0))]
         [(not (equal? t3 TOKEN-ID))    (resultado toks* (add-error (res-errors r0) TOKEN-ID    t3) (res-automata r0))]
         [(not (equal? t4 TOKEN-COLON)) (resultado toks* (add-error (res-errors r0) TOKEN-COLON t4) (res-automata r0))]
         [(not (equal? t5 TOKEN-ID))    (resultado toks* (add-error (res-errors r0) TOKEN-ID    t5) (res-automata r0))]
         [else
          (let* ([from      (cadar          toks*)]
                 [sym       (cadar (cddr    toks*))]
                 [to        (cadar (cddddr  toks*))]
                 [automata* (add-automata (res-automata r0) from 'transicion (list sym to))]
                 [after5    (cdr (cddddr toks*))]
                 [next      (caar after5)])
            (cond
              [(equal? next TOKEN-NEWLINE)
               (syn-deltaPrime (cdr after5) (res-errors r0) automata*)]
              [(equal? next TOKEN-COMMENT)
               (let ([r (syn-comment after5 (res-errors r0) automata*)])
                 (syn-deltaPrime (res-toks r) (res-errors r) (res-automata r)))]
              [else
               (resultado after5 (add-error (res-errors r0) TOKEN-NEWLINE next) automata*)]))]))]
    [else
     (resultado toks* (add-error (res-errors r0) TOKEN-ID ctok) (res-automata r0))]))


(define (syn-deltaPrime toks errors automata)
  (define ctok (caar toks))
  (cond
    [(not (equal? ctok TOKEN-ID))
     (resultado toks errors automata)]  ; epsilon: no hay mas transiciones
    [else
     (let ([t2 (caadr        toks)]
           [t3 (caaddr       toks)]
           [t4 (caar (cdddr  toks))]
           [t5 (caar (cddddr toks))])
       (cond
         [(not (equal? t2 TOKEN-COLON)) (resultado toks (add-error errors TOKEN-COLON t2) automata)]
         [(not (equal? t3 TOKEN-ID))    (resultado toks (add-error errors TOKEN-ID    t3) automata)]
         [(not (equal? t4 TOKEN-COLON)) (resultado toks (add-error errors TOKEN-COLON t4) automata)]
         [(not (equal? t5 TOKEN-ID))    (resultado toks (add-error errors TOKEN-ID    t5) automata)]
         [else
          (let* ([from      (cadar         toks)]
                 [sym       (cadar (cddr   toks))]
                 [to        (cadar (cddddr toks))]
                 [automata* (add-automata automata from 'transicion (list sym to))]
                 [after5    (cdr (cddddr toks))]
                 [next      (caar after5)])
            (cond
              [(equal? next TOKEN-NEWLINE)
               (syn-deltaPrime (cdr after5) errors automata*)]
              [(equal? next TOKEN-COMMENT)
               (let ([r (syn-comment after5 errors automata*)])
                 (syn-deltaPrime (res-toks r) (res-errors r) (res-automata r)))]
              [else
               (resultado after5 (add-error errors TOKEN-NEWLINE next) automata*)]))]))]))


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
        (add-error (res-errors r5) TOKEN-EOF tok-final)))

  (list final-errors (res-automata r5)))

(provide rec-des)
