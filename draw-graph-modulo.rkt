#lang racket


(require racket/system)       ; para el comando en terminal
(require racket/runtime-path) ; para manejo de rutas
(require net/base64)          ; para encoding de imagen

; esto establece la ruta actual
(define-runtime-path here ".")
(current-directory here)
(displayln (format "DIR: ~a" (current-directory)))

; devuelve el contenido del .dot
(define (genera-dot automata-rep)
    (define dot-text
      "digraph DFA {
        rankdir=LR;

        start -> q0 [label=\"\"];
        q0 -> q1 [label=\"a\"];
        q1 -> q2 [label=\"b\"];
        q2 -> q2 [label=\"a,b\"];

        start [shape=plaintext];
        q1 [shape=doublecircle];
        q2 [shape=doublecircle];    }"
    )
    dot-text
)

(define (genera-imagen automata-rep)
    (define dot-text (genera-dot automata-rep))
    (parameterize ([current-directory here])
      (call-with-output-file "dfa.dot" #:exists 'replace
                             (lambda (out) (display dot-text out)))
      (system "dot -Tpng dfa.dot -o dfa.png")
      (displayln "dfa.png generated")
      (bytes->string/latin-1 (base64-encode (file->bytes "dfa.png"))))
)
(provide genera-imagen)
