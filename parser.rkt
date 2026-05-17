#lang racket

; "[q0, q1, q2]" -> '("q0" "q1" "q2")
(define (parse-list-lexema lexema)
  (define inner (substring lexema 1 (- (string-length lexema) 1)))
  (filter (lambda (s) (not (string=? s "")))
          (map string-trim (string-split inner ","))))

; Agrupa el token stream en listas por linea (separadas por "newline")
(define (group-by-newline tokens)
  (let loop ([remaining tokens] [current '()] [groups '()])
    (cond
      [(null? remaining)
       (reverse (if (null? current) groups (cons (reverse current) groups)))]
      [(equal? (first (first remaining)) "newline")
       (loop (rest remaining) '()
             (if (null? current) groups (cons (reverse current) groups)))]
      [else
       (loop (rest remaining) (cons (first remaining) current) groups)])))

; Parsea una linea de tokens a una tupla semantica o #f
(define (parse-line line)
  (cond
    [(null? line) #f]

    [(equal? (first (first line)) "comment_op") #f]

    [(equal? (first (first line)) "states")
     (list 'states (parse-list-lexema (second (third line))))]

    [(equal? (first (first line)) "input_alphabet")
     (list 'alphabet (parse-list-lexema (second (third line))))]

    [(equal? (first (first line)) "start_state_op")
     (list 'start (second (third line)))]

    [(equal? (first (first line)) "accept_states_op")
     (list 'accept (parse-list-lexema (second (third line))))]

    [(equal? (first (first line)) "delta_op") #f]
    
    [(and (equal? (first (first line)) "state_assign_op")
          (>= (length line) 6))
     ; - estado-origen : simbolo : estado-destino
     (list 'transition
           (second (second line))
           (second (fourth line))
           (second (sixth line)))]
    [else #f]))

; Construye hash anidado: estado -> (hash simbolo -> estado-destino)
(define (build-delta transitions)
  (foldl
   (lambda (t acc)
     (let* ([from      (second t)]
            [sym       (string->symbol (third t))]
            [to        (fourth t)]
            [state-map (if (hash-has-key? acc from) (hash-ref acc from) (hash))])
       (hash-set acc from (hash-set state-map sym to))))
   (hash)
   transitions))

; token-stream -> hash del automata compatible con valida
(define (parse-automaton token-stream)
  (define lines       (group-by-newline token-stream))
  (define parsed      (filter values (map parse-line lines)))
  (define (get tag)
    (define found (findf (lambda (p) (equal? (first p) tag)) parsed))
    (and found (second found)))
  (define transitions (filter (lambda (p) (equal? (first p) 'transition)) parsed))
  (define states      (get 'states))
  (define delta       (build-delta transitions))

  (foldl (lambda (state acc)
           (if (hash-has-key? delta state)
               (hash-set acc state (hash-ref delta state))
               acc))
         (hash "iniciales" (get 'start)
               "finales"   (get 'accept)
               "states"    states
               "alphabet"  (get 'alphabet))
         (if states states '())))

(provide parse-automaton)
