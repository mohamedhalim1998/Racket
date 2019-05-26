#lang racket

(provide (all-defined-out)) ;; so we can put tests in a second file

;; put your code below

;; int int int -> int list
;; ASSUME: stride is positive
;; sequence produces a list of numbers from low to high (including
;; low and possibly high) separated by stride and in sorted order
(define (sequence low high stride)
  (if (> low high)
      null
      (cons low (sequence (+ low stride) high stride))))

;; string list  string -> string list
;; append every element in the input string list with the string given
(define (string-append-map xs suffix)
  (map (lambda (x) (string-append  x suffix)) xs))

;; list int -> element
;; 
(define (list-nth-mod xs n)
  (cond [(< n 0) (error "list-nth-mod: negative number")]
        [(null? xs) (error "list-nth-mod: empty list")]
        [#t (car (list-tail xs (remainder n (length xs))))]))
;; procedure int -> list
;; get the first n-th element of the stream
(define (stream-for-n-steps stream n)
  (if (= n 0)
      null
      (cons (car (stream)) (stream-for-n-steps (cdr (stream)) (- n 1)))))

;; no argument -> pair
;; porduce a list of int count-up 
;; if the number is divisiable by 5 give the negative of it
(define funny-number-stream (letrec ([f (lambda (x)
                                          (if (= (remainder x 5) 0)
                                             (cons (- x) (lambda () (f (+ x 1))))
                                             (cons x  (lambda () (f (+ x 1))))))])
                              (lambda () (f 1))))

;; no argument -> pair
(define dan-then-dog (letrec  ([f (lambda (x)
                                    (if (string=?  x "dan.jpg")
                                        (cons "dog.jpg" (lambda () (f "dog.jpg")))
                                        (cons "dan.jpg" (lambda () (f "dan.jpg" )))))])
                       (lambda () (f "dog.jpg"))))

;; stream -> pair
;; produce a pair with the value of the stream and zero

(define (stream-add-zero x) (lambda () (cons (cons 0 (car (x)))
                                             (stream-add-zero (cdr (x))))))


;; list , list -> pair
;; produce a stream with its head is from the given list
(define (cycle-lists xs ys)
  (letrec ([f (lambda (l1  l2)
              (cond [(and (null? l1) (null? l2))
                     (cons (cons (car xs) (car ys)) (lambda () (f (cdr xs) (cdr ys))))]
                    [(null?  l1) (cons (cons (car xs) (car l2)) (lambda () (f (cdr xs)  (cdr l2))))]
                    [(null?  l2) (cons (cons (car l1) (car ys)) (lambda () (f  (cdr l1)   (cdr ys))))]
                    [#t        (cons (cons (car l1) (car l2)) (lambda () (f  (cdr l1)   (cdr l2))))]))])
           (lambda () (f xs ys))))

;; value , vector -> pair
;; return the pair acco with the gevin value in the vector

(define (vector-assoc v vec)
  (letrec ([f (lambda (n)
               (cond [(= n (vector-length vec)) #f]
                     [(equal? v (car (vector-ref vec n))) (vector-ref vec n)]
                     [#t (f (+ n 1))]))])
    (f 0)))
;;(define cache (make-vector 10 #f))
    
(define (cached-assoc xs n)
  (letrec ([cache (make-vector n #f)]
           [index 0]
           [full-cache #f]
           [sub-vector (lambda (x y)
                         (if full-cache
                             cache
                         (if (= x (- y 1))
                             (vector-ref cache x)
                             (vector (vector-ref cache x) (sub-vector (+ x 1) y)))))]
           [cache-it (lambda (v) (let* ([res (assoc v xs)])
                        (begin  (vector-set! cache index res)
                                (if (= index n)
                                    (begin (set! full-cache #t) (set! index 0))
                                    (set! index (+ 1 index))) res)))]
           [f (lambda (v)
                (if (> index 0)
                    (let ([r (vector-assoc v (sub-vector 0 index))])
                    (if r
                        r
                        (cache-it v)))
                    (cache-it v)))])
                        
        f))

;; macro define
(define-syntax while-less
  (syntax-rules (do)
    [(while-less e1 do e2)
      (let* ([d e1])
        (letrec ([s e2] [f (lambda () (if (>= s d)
                                   (begin s #t)
                                   (begin (set! s e2) (f))))])

         (f)))]))














