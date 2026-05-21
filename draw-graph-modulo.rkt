#lang racket

(require racket/system)
(require racket/runtime-path)
(require net/base64)

(define-runtime-path here ".")
(current-directory here)
(displayln (format "DIR: ~a" (current-directory)))

; Genera las lineas de transicion: q0 -> q1 [label="a"];
(define (transition-lines automata states)
  (apply string-append
         (map (lambda (state)
                (if (hash-has-key? automata state)
                    (let ([delta-map (hash-ref automata state)])
                      (apply string-append
                             (map (lambda (sym)
                                    (format "  ~a -> ~a [label=\"~a\"];\n"
                                            state
                                            (hash-ref delta-map sym)
                                            (symbol->string sym)))
                                  (hash-keys delta-map))))
                    ""))
              states)))

; Genera las declaraciones de forma de cada estado
(define (shape-lines states finals)
  (apply string-append
         (map (lambda (state)
                (if (member state finals)
                    (format "  ~a [shape=doublecircle];\n" state)
                    (format "  ~a [shape=circle];\n" state)))
              states)))

; automata hash -> string en formato .dot
(define (genera-dot automata)
  (define start (hash-ref automata "iniciales" "?"))
  (define finals (hash-ref automata "finales" '()))
  (define states (hash-ref automata "states" '()))
  (string-append "digraph DFA {\n"
                 "  rankdir=LR;\n"
                 (format "  start -> ~a [label=\"\"];\n" start)
                 (transition-lines automata states)
                 "  start [shape=plaintext];\n"
                 (shape-lines states finals)
                 "}\n"))

; automata hash -> string base64 del PNG generado
(define (genera-imagen automata)
  (define dot-text (genera-dot automata))
  (parameterize ([current-directory here])
    (call-with-output-file "dfa.dot" #:exists 'replace (lambda (out) (display dot-text out)))
    (system "dot -Tpng dfa.dot -o dfa.png")
    (displayln "dfa.png generated")
    (bytes->string/latin-1 (base64-encode (file->bytes "dfa.png")))))

(provide genera-imagen)
