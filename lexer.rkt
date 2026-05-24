#lang racket

;funciones para obtener tokens

(define reMatch regexp-match-positions)
(define TOKEN-COMA          "comma_op")
(define TOKEN-ID            "ID")
(define TOKEN-COLON         "colon_op")
(define TOKEN-START-STATE   "start_state_op")
(define TOKEN-ACCEPT-STATES "accept_states_op")
(define TOKEN-KW-STATE      "states")
(define TOKEN-ALPHABET      "input_alphabet")
(define TOKEN-DELTA         "delta_op")
(define TOKEN-EOF           "EOF")
(define TOKEN-STATE         "state")
(define TOKEN-NEWLINE "newline")
(define TOKEN-COMMENT "comment_op")
(define TOKEN-ERROR "error")


(define (getMatch label-regex str)
  (define label (first label-regex))
  (define regex (second label-regex))
  (define match (reMatch regex str))

  (if match
      (let* ([lenMatch (cdr (first match))]
             [subStr (substring str 0 lenMatch)])
        (list label lenMatch subStr))
      (list "none" 0 "")))

(define (getMaximo allMatches)
  (let* ([longs (map second allMatches)]
         [maxLen (apply max longs)]
         [es-max? (lambda (row) (= maxLen (second row)))])
    (first (filter es-max? allMatches))))

(define allRegex
  (list
   (list TOKEN-COMMENT #rx"^#[^\n]*")
   (list TOKEN-ALPHABET #rx"^input_alphabet")
   (list TOKEN-KW-STATE #rx"^states")
   (list TOKEN-DELTA #rx"^delta")
   (list TOKEN-START-STATE #rx"^start_state")
   (list TOKEN-ACCEPT-STATES #rx"^accept_states")
   (list TOKEN-COLON #rx"^:")
   (list TOKEN-COMA #rx"^,")
   (list TOKEN-NEWLINE #rx"^\n")
   (list TOKEN-STATE #rx"^q[0-9]+")
   (list TOKEN-ID    #rx"^[a-zA-Z0-9]")))

(define (trim-spaces str)
  (string-trim str #px"[ \t\r]+"))

(define (tokenize-line str)
  (let loop ([remaining (trim-spaces str)]
             [tokens '()]
             [num-linea 1])
    (cond
      [(string=? remaining "") (reverse tokens)]
      [else
       (define allMatches (map (lambda (r) (getMatch r remaining)) allRegex))
       (define best (getMaximo allMatches))
       (if (= (second best) 0)
           (loop (substring remaining 1) (cons (list TOKEN-ERROR (substring remaining 0 1) num-linea) tokens) num-linea)

           (let ([nueva-linea (if (equal? (first best) TOKEN-NEWLINE)
                                  (+ num-linea 1)
                                  num-linea)])
             (loop (trim-spaces (substring remaining (second best)))
                   (cons (list (first best) (third best) num-linea) tokens)
                   nueva-linea)))])))

(define (tokenize-all input)
  (define tokens (tokenize-line input))
  (define eof-line
    (if (null? tokens)
        1
        (let ([ultimo (last tokens)])
          (if (>= (length ultimo) 3) (third ultimo) 1))))
  (append tokens (list (list TOKEN-EOF TOKEN-EOF eof-line))))

;funcion para formatizar tokens en html
(define (styler token)
  (define label (first token))
  (define lexema (second token))
  (cond
    [(equal? label TOKEN-NEWLINE) "<br>\n"]
    [(equal? label TOKEN-EOF)     ""]
    [else (string-append "<text class='" label "'>" lexema "</text> ")]))

(define (tokens->html token-stream)
  (define body (apply string-append (map styler token-stream)))
  (string-append
   "<!DOCTYPE html>\n<html>\n<head>\n<style>\n"
   "  body             { font-family: monospace; background:#1e1e1e; color:white; padding:20px; }\n"
   "  .comment_op      { color:#6a9955; font-style:italic; }\n"
   "  .input_alphabet  { color:#c586c0; font-weight:bold; }\n"
   "  .states          { color:#569cd6; font-weight:bold; }\n"
   "  .delta_op        { color:#c586c0; font-weight:bold; }\n"
   "  .start_state_op  { color:#569cd6; font-weight:bold; }\n"
   "  .accept_states_op{ color:#569cd6; font-weight:bold; }\n"
   "  .state_assign_op { color:#d4d4d4; }\n"
   "  .colon_op        { color:#d4d4d4; }\n"
   "  .comma_op        { color:#d4d4d4; }\n"
   "  .list_op         { color:#4ec9b0; }\n"
   "  .string          { color:#ce9178; }\n"
   "  .ASCII           { color:#dcdcaa; }\n"
   "  .ID              { color:#9cdcfe; }\n"
   "  .error              { color: red; }\n"
   "</style>\n</head>\n<body>\n\t"
   body
   "\n</body>\n</html>"))

(define (error-tokens? token-stream)
  (findf (lambda (token) (equal? (first token) TOKEN-ERROR)) token-stream))

(define (clean-token-stream token-stream)
  (filter (lambda (tok)
            (not (member (first tok) (list TOKEN-COMMENT TOKEN-NEWLINE))))
          token-stream))


(provide tokenize-line
         tokenize-all
         tokens->html
         error-tokens?
         clean-token-stream)
(provide TOKEN-COMA
         TOKEN-ID
         TOKEN-COLON
         TOKEN-START-STATE
         TOKEN-ACCEPT-STATES
         TOKEN-KW-STATE
         TOKEN-ALPHABET
         TOKEN-DELTA
         TOKEN-EOF
         TOKEN-STATE
         TOKEN-NEWLINE
         TOKEN-COMMENT
         tokenize-all)

