#lang racket
;; hang-man for REPL Scheme

;;
(define source-name "glossary.txt")

;; Side effect:
;; Strig-> IO([String])
;; Passed the path, open the file containig glossary
(define (read-words-from filename)
  (let* ((port (open-input-file filename))
         (res (read-word-list port '())))
    (close-input-port port)
    res))

;; Side effect
;; Fd -> [String] -> IO ([String])
;; Passed port and acumulator, return the all the words as strings
(define (read-word-list port acc)
  (let ((stuff (read port)))
    (if (eof-object? stuff)
        acc
        (read-word-list port
                        (cons (symbol->string stuff) acc)))))

(define list-of-words (read-words-from source-name))

;; STATE OF THE GAME
(define word-to-guess null)
(define partial-sol null)

(define hits 0)
(define plays 0)
(define failures 0)
(define total-failures 6)
(define total-hits (length word-to-guess))
(define glossary (map string->list list-of-words))


;; 
;; IO(String)
(define (game-status)
  (begin
    (format "~a H:~a/~a F:~a/~a ~a ~a"
            (list->string partial-sol)
            hits  total-hits
            failures  total-failures
            plays
            (if (and
                 (< hits total-hits)
                 (< failures total-failures))
                ""
                (string-append "GAME-OVER(" (list->string word-to-guess) ")")))))

          

;;;
;;  PURELY FUNCTIONAL
;;

(define (occurrences word char)
  (if (null? word)
      0
      (if (eq? char (car word))
          (+ 1 (occurrences (cdr word) char))
          (occurrences (cdr word) char))))

(define (indices word char)
  (for/list ([i (in-naturals 0)]
             [c word]
        #:when (eq? c char))
    i))
  
  
(define (replace-indices word idx new)
  (for/fold ([ans word])
            ([i idx])
    (list-set ans i new)))
    


(define (noOfHits hidden)
  (for/fold ([n 0])
            ([c hidden]
             #:when (not (eq? c #\*)))
            (+ n 1)))


;; Side effects
;; IO(String)
(define (restart)
  (begin
    (set! word-to-guess (list-ref glossary (random (length glossary))))
    (set! partial-sol (string->list (make-string (length word-to-guess) #\*)))
    (set! hits 0)
    (set! plays 0)
    (set! failures 0)
    (set! total-failures 6)
    (set! total-hits (length word-to-guess))
    (game-status)))


;; Char -> IO(String)
(define (guess char)
  (begin
    (set! plays (+ plays 1))
    (set! partial-sol
          (let ([idx (indices word-to-guess char)])
          (replace-indices partial-sol idx char)))
    (set! hits (noOfHits partial-sol))
    (if (zero? (occurrences word-to-guess char))
        (set! failures (+ failures 1))
        (void))
    (game-status)))


;; IO(String)
(define (solve word)
  (begin
    (for ([w word])
      (guess w)
    )
    (game-status)))


;;
;; EXTRA -F3
;;;;;;;;;;;;;;;;;;;;;;;;;;
   
;; p: all-words as list of list of char
(define (words-containing all-words char)
  (for/list ([wl all-words]
        #:when (not (zero? (occurrences wl char))))
    wl))


;; p: all-words as list of list of char
;;  : chars as a list of char
(define (words-containing-ext all-words chars)
  (for/list ([wl all-words]
             #:when (for/and
                         ([char chars])
                       (not (zero? (occurrences wl char)))))
    wl))

;; IO([String])
;; this is very hard.
(define (sieve chars)
  (begin (map list->string (words-containing-ext glossary chars))))
