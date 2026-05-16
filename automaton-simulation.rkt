#lang racket

;(require "tokenizar.rkt")


(define (findN lis n) (cond [(empty? lis) #f]
                            [( = n (first lis)) #t]
                            [else (findN (cdr lis) n)]))
(findN '(1 2 3 4 5 6 7) 12)

; representar-automata: lista-de-tokens hash -> hash
; Recibe los tokens de UNA línea y el hash acumulado hasta ahora.
; Devuelve el hash actualizado según lo que indique la línea.
(define (representar-automata tokens acc)
  (cond
    ; línea vacía o comentario → no cambia nada
    [(null? tokens) acc]
    [(equal? (first (first tokens)) "comment_op") acc]

    ; states: [q0, q1, q2]
    [(equal? (first (first tokens)) "states")
     (let ([ids (filter (lambda (t) (equal? (first t) "Id")) tokens)])
       (hash-set acc "estados" (map second ids)))]

    ; input_alphabet: [a, b]
    [(equal? (first (first tokens)) "input_alphabet")
     (let ([ids (filter (lambda (t) (equal? (first t) "Id")) tokens)])
       (hash-set acc "alfabeto" (map second ids)))]

    ; start_state: q0
    [(equal? (first (first tokens)) "start_state_op")
     (let ([id (findf (lambda (t) (equal? (first t) "Id")) tokens)])
       (if id (hash-set acc "iniciales" (second id)) acc))]

    ; accept_states: [q2]
    [(equal? (first (first tokens)) "accept_states_op")
     (let ([ids (filter (lambda (t) (equal? (first t) "Id")) tokens)])
       (hash-set acc "finales" (map second ids)))]

    ; - q0 : a -> q1   (delta)
    [(equal? (first (first tokens)) "state_assign_op")
     (let* ([ids      (map second (filter (lambda (t) (equal? (first t) "Id")) tokens))]
            [desde    (if (>= (length ids) 1) (list-ref ids 0) #f)]
            [simbolo  (if (>= (length ids) 2) (string->symbol (list-ref ids 1)) #f)]
            [hacia    (if (>= (length ids) 3) (list-ref ids 2) #f)]
            [delta    (hash-ref acc "delta" (hash))]
            [fila     (hash-ref delta desde (hash))])
       (if (and desde simbolo hacia)
           (hash-set acc "delta" (hash-set delta desde (hash-set fila simbolo hacia)))
           acc))]

    [else acc]))



(define (valida automata cadena)
  (let* ([iniciales (hash-ref automata "iniciales")]
         [finales   (hash-ref automata "finales")])

    (define (helper current cadena)
      (let* ([esFinal       (member current finales)]
             [esVacia       (equal? cadena "")]
             [primerChar    (if esVacia #f (string->symbol (substring cadena 0 1)))]
             [hayTransicion (and primerChar
                                 (hash-has-key? automata current)
                                 (hash-has-key? (hash-ref automata current) primerChar))])
        (cond
          [(and esFinal esVacia)       #t]
          [(and (not esFinal) esVacia) #f]
          [(not hayTransicion)         #f]
          [else
           (helper (hash-ref (hash-ref automata current) primerChar)
                   (substring cadena 1))])))

    (helper iniciales cadena)))


    (provide valida representar-automata)

