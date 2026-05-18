#lang racket

;; ── Utilidades ────────────────────────────────────────────────────────────────

(define (token-label  t) (first  t))
(define (token-lexema t) (second t))

(define (skip-newlines tokens)
  (cond [(null? tokens) '()]
        [(equal? (token-label (first tokens)) "newline") (skip-newlines ( tokens))]
        [else tokens]))

(define (is-id? t)
  (member (token-label t) '("Id" "ASCII")))

(define (parse-list-lexema lexema)
  (define inner (substring lexema 1 (- (string-length lexema) 1)))
  (filter (lambda (s) (not (string=? s "")))
          (map string-trim (string-split inner ","))))

;; ── Funciones expect (lanzan error si no coincide) ───────────────────────────

(define (expect tokens label)
  (cond
    [(null? tokens)
     (error (format "Error de sintaxis: se esperaba '~a' pero se llego al final" label))]
    [(not (equal? (token-label (first tokens)) label))
     (error (format "Error de sintaxis: se esperaba '~a' pero se encontro '~a'"
                    label (token-lexema (first tokens))))]
    [else (values (first tokens) (rest tokens))]))

(define (expect-id tokens)
  (cond
    [(null? tokens)
     (error "Error de sintaxis: se esperaba un identificador pero se llego al final")]
    [(not (is-id? (first tokens)))
     (error (format "Error de sintaxis: se esperaba identificador pero se encontro '~a'"
                    (token-lexema (first tokens))))]
    [else (values (token-lexema (first tokens)) (rest tokens))]))

(define (expect-newline tokens)
  (let* ([es-null    (null? tokens)]
         [es-newline (and (not es-null) (equal? (token-label (first tokens)) "newline"))])
    (if (or es-null es-newline)
        (values #f (if es-null '() (rest tokens)))
        (error (format "Error de sintaxis: se esperaba fin de linea pero se encontro '~a'"
                       (token-lexema (first tokens)))))))

;; ── Parseo de cada seccion ────────────────────────────────────────────────────

(define (parse-comment tokens)
  (define-values (_  r1) (expect tokens "comment_op"))
  (define-values (__ r2) (expect-newline r1))
  (values '() r2))

(define (parse-states-line tokens)
  (define-values (_   r1) (expect tokens "states"))
  (define-values (__  r2) (expect r1 "colon_op"))
  (define-values (lst r3) (expect r2 "list_op"))
  (define-values (___ r4) (expect-newline r3))
  (let* ([texto-lista    (token-lexema lst)]
         [nombres-estados (parse-list-lexema texto-lista)]
         [seccion-states  (list 'states nombres-estados)])
    (values (list seccion-states) r4)))

(define (parse-alphabet-line tokens)
  (define-values (_   r1) (expect tokens "input_alphabet"))
  (define-values (__  r2) (expect r1 "colon_op"))
  (define-values (lst r3) (expect r2 "list_op"))
  (define-values (___ r4) (expect-newline r3))
  (let* ([texto-lista    (token-lexema lst)]
         [simbolos-alfabeto (parse-list-lexema texto-lista)]
         [seccion-alfabeto  (list 'alphabet simbolos-alfabeto)])
    (values (list seccion-alfabeto) r4))
)

(define (parse-start-line tokens)
  (define-values (_   r1) (expect tokens "start_state_op"))
  (define-values (__  r2) (expect r1 "colon_op"))
  (define-values (id  r3) (expect-id r2))
  (define-values (___ r4) (expect-newline r3))
  (let ([seccion-start (list 'start id)])
    (values (list seccion-start) r4)))

(define (parse-accept-line tokens)
  (define-values (_   r1) (expect tokens "accept_states_op"))
  (define-values (__  r2) (expect r1 "colon_op"))
  (define-values (lst r3) (expect r2 "list_op"))
  (define-values (___ r4) (expect-newline r3))
  (let* ([texto-lista        (token-lexema lst)]
         [estados-aceptacion (parse-list-lexema texto-lista)]
         [seccion-accept     (list 'accept estados-aceptacion)])
    (values (list seccion-accept) r4)))

(define (parse-transition tokens)
  (define-values (_    r1) (expect    tokens "state_assign_op"))
  (define-values (from r2) (expect-id r1))
  (define-values (__   r3) (expect    r2 "colon_op"))
  (define-values (sym  r4) (expect-id r3))
  (define-values (___  r5) (expect    r4 "colon_op"))
  (define-values (to   r6) (expect-id r5))
  (define-values (____ r7) (expect-newline r6))
  (let ([seccion-transicion (list 'transition from sym to)])
    (values (list seccion-transicion) r7)))

; Parsea cero o mas transiciones mientras el siguiente token sea "-"
(define (parse-transitions tokens)
  (if (or (null? tokens)
          (not (equal? (token-label (first tokens)) "state_assign_op")))
      (values '() tokens)
      (let-values ([(trans r1) (parse-transition tokens)])
        (let-values ([(rest-trans r2) (parse-transitions (skip-newlines r1))])
          (values (append trans rest-trans) r2)))))

(define (parse-delta-block tokens)
  (define-values (_   r1) (expect tokens "delta_op"))
  (define-values (__  r2) (expect r1 "colon_op"))
  (define-values (___ r3) (expect-newline r2))
  (define-values (transitions r4) (parse-transitions (skip-newlines r3)))
  (values transitions r4))

;; ── Despachador principal ─────────────────────────────────────────────────────

(define (parse-section tokens)
  (define label (token-label (first tokens)))
  (cond
    [(equal? label "comment_op")       (parse-comment       tokens)]
    [(equal? label "states")           (parse-states-line   tokens)]
    [(equal? label "input_alphabet")   (parse-alphabet-line tokens)]
    [(equal? label "start_state_op")   (parse-start-line    tokens)]
    [(equal? label "accept_states_op") (parse-accept-line   tokens)]
    [(equal? label "delta_op")         (parse-delta-block   tokens)]
    [else (error (format "Error de sintaxis: seccion desconocida '~a'"
                         (token-lexema (first tokens))))]))

; Parsea secciones recursivamente hasta agotar los tokens
(define (parse-sections tokens)
  (if (null? tokens)
      (values '() '())
      (let-values ([(new-sections remaining) (parse-section tokens)])
        (let-values ([(rest-sections final) (parse-sections (skip-newlines remaining))])
          (values (append new-sections rest-sections) final)))))

;; ── Construccion del hash del automata ───────────────────────────────────────

(define (build-delta transitions)
  (foldl (lambda (t acc)
           (let* ([from      (second t)]
                  [sym       (string->symbol (third t))]
                  [to        (fourth t)]
                  [state-map (if (hash-has-key? acc from) (hash-ref acc from) (hash))])
             (hash-set acc from (hash-set state-map sym to))))
         (hash)
         transitions))

(define (build-automaton sections)
  (define (get tag)
    (define found (findf (lambda (s) (equal? (first s) tag)) sections))
    (and found (second found)))
  (define transitions (filter (lambda (s) (equal? (first s) 'transition)) sections))
  (define states (get 'states))
  (define delta  (build-delta transitions))
  (foldl (lambda (state acc)
           (if (hash-has-key? delta state)
               (hash-set acc state (hash-ref delta state))
               acc))
         (hash "iniciales" (get 'start)
               "finales"   (get 'accept)
               "states"    states
               "alphabet"  (get 'alphabet))
         (if states states '())))

;; ── Funcion principal: devuelve ('ok hash) o ('syntax-error mensaje) ─────────

(define (parse-automaton token-stream)
  (with-handlers ([exn:fail? (lambda (e) (list 'syntax-error (exn-message e)))])
    (define-values (sections _) (parse-sections (skip-newlines token-stream)))
    (list 'ok (build-automaton sections))))

(provide parse-automaton)
