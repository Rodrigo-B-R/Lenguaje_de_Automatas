#lang racket

;funciones para obtener tokens

(define reMatch regexp-match-positions)
(require "automaton-simulation.rkt")




(define (getMatch label-regex str)
  (define label (first label-regex))
  (define regex (second label-regex))
  (define match (reMatch regex str )  )
  
   ; Si hay match 
   ;    devuelve una lista asi: (list label lenMatch subStr)
   ;    si no,  una lista asi:  (list "none" 0 "")
  (if match
      (let* 
          ( [lenMatch  (cdr (first match)) ]
            [subStr   (substring  str  0 lenMatch ) ] 
           )
          (list label lenMatch subStr)
      )
      (list "none" 0 "")
  )
)

(define (getMaximo allMatches)
  ( let*
       ( [longs (map second allMatches)]
         [maxLen (apply max longs) ]
         [es-max? (lambda (row) (= maxLen (second row) ) ) ] 
       )
     (first (filter es-max? allMatches) )
   )
 )


(define allRegex
  (list
    (list "comment_op"       #rx"^#.*")
    (list "input_alphabet"   #rx"^input_alphabet")
    (list "states"           #rx"^states")
    (list "delta_op"         #rx"^delta")
    (list "start_state_op"   #rx"^start_state")
    (list "accept_states_op" #rx"^accept_states")
    (list "state_assign_op"  #rx"^-")
    (list "colon_op"         #rx"^:")
    (list "comma_op"         #rx"^,")
    (list "list_op"          #px"^\\[\\s*[a-zA-Z][a-zA-Z0-9]*(\\s*,\\s*[a-zA-Z][a-zA-Z0-9]*)*\\s*\\]")
    (list "string"           #rx"^\"[^\"]*\"")
    (list "ASCII"            #rx"^[a-zA-Z]+")
    (list "Id"               #rx"^[a-zA-Z][a-zA-Z0-9]*")
  ))


(define (tokenize-line str)
  (let loop ([remaining (string-trim str)] [tokens '()])
    (cond
      [(string=? remaining "") (reverse tokens)]
      [else
       (define allMatches (map (lambda (r) (getMatch r remaining)) allRegex))
       (define best (getMaximo allMatches))
       (if (= (second best) 0)
           
           ;no hubo match
           (loop (substring remaining 1) (cons (list "error" (substring remaining 0 1)) tokens)) 

            ;si hubo match
           (loop (string-trim (substring remaining (second best))) 
                 (cons (list (first best) (third best)) tokens))) ])))

(define (tokenize-all lines)
  (apply append
         (map (lambda (line)
                (append (tokenize-line line) '(("newline" "<br>"))))
              lines)))





;funcion para formatizar tokens en html
(define (styler token)
  (define label  (first token))
  (define lexema (second token))
  (if (equal? label "newline")
      "<br>\n"
      (string-append "<text class='" label "'>" lexema "</text> ")))

(define (tokens->html token-stream)
  (define body (apply string-append (map styler token-stream)))
  (string-append
   "<!DOCTYPE html>\n<html>\n<head>\n<style>\n"
   "  body             { font-family: monospace; background:#1e1e1e; color:white; padding:20px; }\n"
   "  .comment_op      { color:#6a9955; font-style:italic; }\n"
   "  .input_alphabet  { color:#c586c0; font-weight:bold; }\n"
   "  .states          { color:#569cd6; font-weight:bold; }\n"
   "  .delta_op        { color:#569cd6; font-weight:bold; }\n"
   "  .start_state_op  { color:#569cd6; font-weight:bold; }\n"
   "  .accept_states_op{ color:#569cd6; font-weight:bold; }\n"
   "  .state_assign_op { color:#d4d4d4; }\n"
   "  .colon_op        { color:#d4d4d4; }\n"
   "  .comma_op        { color:#d4d4d4; }\n"
   "  .list_op         { color:#4ec9b0; }\n"
   "  .string          { color:#ce9178; }\n"
   "  .ASCII           { color:#dcdcaa; }\n"
   "  .Id              { color:#9cdcfe; }\n"
   "  .error              { color: red; }\n"

   "</style>\n</head>\n<body>\n\t"
   body
   "\n</body>\n</html>"))

(define (error-tokens? token-stream) (findf (lambda (token) (equal? (first token) "error")) token-stream))







   (provide tokenize-line tokenize-all tokens->html error-tokens?)
