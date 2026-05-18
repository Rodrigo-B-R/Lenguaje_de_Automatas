#lang racket

(define (findN lis n) (cond [(empty? lis) #f]
                            [( = n (first lis)) #t]
                            [else (findN (cdr lis) n)]))
(findN '(1 2 3 4 5 6 7) 12)


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


(provide valida)

