"B.4 特殊形式"
(print "P.411")
(define linear
  (lambda (a b x)
    (+ (* a x) b)))
(print (linear 2 3 5))

;;; (Gauche では) この書き方だと let の中に置けない
(define (linear a b x)
  (+ (* a x) b))
(linear 2 3 5)
(define (linear1 u v w)
  (+ (* u w) v))
(linear1 2 3 5)

"P.414"
(define a 3.14)
a

"P.416"
(* 2 a)
(linear 2 3 5)

((lambda (a b x)
   (+ (* a x) b))
 2 3 5)

"P.417"
(define prefix (lambda (proc a b)
                 (proc a b)))
(prefix + 2 3)
(prefix * 2 3)

(define infix
  (lambda (a proc b)
    (proc a b)))
(infix 2 + 3)

(define postfix
  (lambda (a b proc)
    (proc a b)))
(postfix 2 3 +)

"P.418"
(define (linear x)
  (define a 2)
  (define b 3)
  (+ (* a x) b))
(linear 5)

(define (linear x)
  (define a 2)
  (define b 3)
  (+ (* a x) b)
  (* a b))
(linear 5)

"P.419"
(begin
  (define a 2)
  (define b 3)
  (* a b))

"P.420"
((lambda (x)
   ((lambda (a b)
      (+ (* a x) b)) 2 3))
 5)
(define f_2x+3
  (lambda (x)
    ((lambda (a b)
       (+ (* a x) b)) 2 3)))
(f_2x+3 5)
(define f_2x+3
  (lambda (x)
    (let ((a 2) (b 3))
      (+ (* a x) b))))
(f_2x+3 5)

(lambda (x)
  ((lambda (a b)
     (+ (* a x) b)) 2 3))

"B.4.4 引数の書法"
"P.421"
(lambda (x y z)
  (+ (* x x) (* y y) (* z z)))
((lambda (x y z)
   (+ (* x x) (* y y) (* z z)))
 2 3 5)

"P.422"
((lambda (x y . z)
   (+ (* x x) (* y y) (* 5 5)))
 2 3)
((lambda (x y . z)
   (+ (* x x) (* y y)))
 2 3)
((lambda (x . w)
   (+ (* x x) (* 3 3) (* 5 5)))
 2)

"P.423"
(cdr '(3.14 (2 3) (5 7)))

"P.424"
(car '((2 3)))
(cdr '((2 3)))

(define (r-square x y . z)
  (+ (* x x) (* y y) (* 5 5)))
(r-square 2 3)

"リストの生成"
(lambda x x)
((lambda x x) 1 2 3)
(define list (lambda x x))
(define (list . x) x)

"P.425"
((lambda x x))
(list)

"B.4.5 引数のない関数"
"P.425"
(lambda (x) (* 2 3))
((lambda (x) (* 2 3)) 99)
((lambda (x) (* 2 3)) "yes")

((lambda () * 2 3))
(define late (lambda () (* 2 3)))
(define now (* 2 3))

"P.426"
now
late
(late)

(define (late) (* 2 3))
(define now (* 2 3))
late
now

(delay (* 2 3))
(force (delay (* 2 3)))

"B.4.6 抽象化の問題"
"P.426"
(define five (lambda (x) (* 5 x)))
(five 7)
(define five (lambda (y) (+ y y y y y)))
(five 7)
(define five (lambda (z) (/ (* 10 z) 2)))
(five 7)

"B.4.7 quote"
"P.428"
(quote Gauss)
(+ 2 3)
(quote (+ 2 3))

"P.429"
(eval (quote (+ 2 3)) (interaction-environment))

((car    (list + - * /)) 5 3)
((cadr   (list + - * /)) 5 3)
((caddr  (list + - * /)) 5 3)
((cadddr (list + - * /)) 5 3)

"P.430"
(cons 7 (quote ()))
(cons (quote b) (quote ()))
(cons (quote a) (cons (quote b) (quote ())))
(cons 'a (cons 'b ()))
(equal? (cons 'a (cons 'b ()))
        (cons (quote a) (cons (quote b) (quote ()))))

'(+ 2 3)
(list 'a 'b 'c)

"P.431"
(append '(a b) '(c d))

(cons '(a b) '(c d))
(append '((a b)) '(c d))
(equal?
 (cons '(a b) '(c d))
 (append '((a b)) '(c d)))
(car (list 'a 'b 'c))
(cdr (list 'a 'b 'c))

(car (quote (quote Euler)))
(car ''Euler)

"B.4.8 内部と外部"
(lambda () scope)
;;; エラーになる
;((lambda () scope))

(define scope 'external)
((lambda () scope))

"P.432"
((lambda ()
   (let ((scope 'internal))
     scope)))
scope

"変数の視野"
((lambda ()
   (let ((scope 'outer))
     (let ((scope 'inner))
       scope))))

"P.433"
((lambda ()
   (let ((x 0))
     (let ((x (+ x 3)))
       (let ((x (+ x 2)))
         (let ((x (+ x 1)))
           x))))))
((lambda ()
   (let ((x_3 0))
     (let ((x_2 (+ x_3 3)))
       (let ((x_1 (+ x_2 2)))
         (let ((x_0 (+ x_1 1)))
           x_0))))))

"P.434"
(let ((times2 (lambda (x) (* x 2)))
      (times3 (lambda (x) (* x 3))))
  (times3 (times2 2)))

(let ()
  (define times2 (lambda (x) (* x 2)))
  (define times3 (lambda (x) (* x 3)))
  (times3 (times2 2)))
((lambda ()
   (define times2 (lambda (x) (* x 2)))
   (define times3 (lambda (x) (* x 3)))
   (times3 (times2 2))))
(begin
  (define times2 (lambda (x) (* x 2)))
  (define times3 (lambda (x) (* x 3)))
  (print (times3 (times2 2)))
  ((compose times3 times2) 2))

"P.435"
;;; エラー
;(let ((x 2) (y 3) (z (* x y)))
;  z)
(let ((x 2) (y 3))
  (let ((z (* x y)))
    z))
;;; 上記コードと等価
(let* ((x 2) (y 3) (z (* x y)))
  z)
(let* ((a 1) (b -5) (c 6)
       (div (* 2 a))
       (minus-b (- 0 b))
       (d (- (* b b) (* 4 a c)))
       (sqr-d (sqrt d))
       (x1 (/ (+ minus-b sqr-d) div))
       (x2 (/ (- minus-b sqr-d) div)))
  (list x1 x2))

(letrec ((z (lambda () (* x y)))
         (x 2)
         (y 3))
  z)

"P.436"
"B.4.9 set!"
(let ((z 0))
  (print z)
  (set! z 2))
(let ((z 0))
  (set! z (* 2 3)))

"P.437"
(define *z* 5)
((lambda ()
   (let ((*z* 0))
     (print *z*)
     (set! *z* (* 2 3))
     (print *z*))
   *z*))
((lambda ()
   (set! *z* (* 3 5))))
*z*
((lambda ()
   (set! *z* (* 3 5))
   *z*))

"P.438"
(let ((x 0) (y 0) (z 0))
  ((lambda ()
    (set! x 2)
    (set! y 3)
    (set! z 5)
    (print x))))

"大域環境によるカウンタ"
(define n 0)
(set! n (+ n 1))
(set! n (+ n 1))
(set! n (+ n 1))
(define count
  (lambda ()
    (set! n (+ n 1))))
(count)
(count)
(count)
(count)

"P.439"
"内部環境によるカウンタ"
(define count
  ((lambda (n)
     (lambda () (set! n (+ n 1))))
   0))
(count)
(define count2
  (let ((n 0))
    (lambda () (set! n (+ n 1)))))
(count2)

"P.440"
"generator"
(define generator
  (lambda ()
    (let ((n 0))
      (lambda () (set! n (+ n 1))))))
(define count-1 (generator))
(define count-2 (generator))
(count-1)
(count-2)
(define generator
  (lambda (initial)
    (let ((n initial))
      (lambda () (set! n (+ n 1))))))
(define count-1 (generator -1))
(define count-2 (generator 0))
(count-1)
(count-2)

"P.441"
"B.4.10 if"
(if (> (* 13 13) (* 8 21))
    "OK"
    "NG")

"P.442"
(if (> (* 13 13) (* 8 21)) "OK"
    "NG")
(define abs (lambda (x)
              (if (< x 0)
                  (- x)
                  x)))
(abs 1)
(abs -1)
(positive? -1)
(positive? 0)
(positive? 1)
(zero? -1)
(zero? 0)
(zero? 1)
(negative? -1)
(negative? 0)
(negative? 1)

"P.443"
(define abs
  (lambda (x)
    (if (negative? x)
        (- x) x)))
(abs -1)
(abs 0)
(abs 1)

(define fact
  (lambda (n)
    (if (zero? n)
        1
        (* n (fact (- n 1))))))
(fact 4)
(letrec ((fact
          (lambda (n)
            (if (zero? n)
                1
                (* n (fact (- n 1)))) )))
  (fact 10))

"B.4.11 特殊形式の意味"
(define Card-1
  (lambda (x)
    (if (= x 1234)
        'ok!
        'oops!)))
"P.444"
(Card-1 1234)
(Card-1 12345)

"cond"
(define abs
  (lambda (x)
    (cond ((positive? x) x)
          ((zero? x) x)
          ((negative? x) (- x)))))
(abs -1)
(abs 0)
(abs 1)

"P.445"
(define abs
  (lambda (x)
    (cond ((negative? x) (- x))
          (else x))))
(abs -1)
(abs 0)
(abs 1)

"condによるif"
(define my-if
  (lambda (predi consq altna)
    (cond (predi consq)
          (else altna))))
(define Card-2
  (lambda (x)
    (my-if (= x 1234)
           'ok!
           oops!)))
;(Card-2 1234) ; エラー
;(Card-2 12345) ; エラー
(define Card-3
  (lambda (x)
    (my-if (= x 1234)
           (lambda () 'ok!)
           (lambda () oops!))))
((Card-3 1234))
(Card-3 12345)

"P.446"
;;; Gauche だと本の意図通りに動かない?
;(define my-if2
;  (lambda (predi consq altna)
;    (cond (predi (lambda () consq))
;          (else (lambda () altna)))))
;(define Card-31
;  (lambda (x)
;    (my-if2 (= x 1234)
;            'ok!
;            oops!)))
;(Card-31 1234)

"andとor"
(define doml-9
  (lambda (x) (and (< 1 x) (< x 9))))
(map (lambda (x)
       (list x (doml-9 x)))
     '(0 1 2 3 4 5 6 7 8 9))

"P.447"
(define or-less5
  (lambda (x) (or (< x 5) (= x 5))))
(map (lambda (x) (list x (or-less5 x))) '(4 5 6 7))

(define or-more5
  (lambda (x) (or (> 5 x) (= x 5))))
(map (lambda (x) (list x (or-more5 x))) '(4 5 6 7))

(define ol5-S (lambda (x) (<= x 5)))
(define om5-S (lambda (x) (>= x 5)))
(map (lambda (x) (list x (ol5-S x))) '(4 5 6 7))
(map (lambda (x) (list x (om5-S x))) '(4 5 6 7))

(define not5 (lambda (x) (not (= x 5))))
(define out5 (lambda (x) (or (< x 5) (< 5 x))))
(map (lambda (x) (list x (not5 x))) '(4 5 6))
(map (lambda (x) (list x (out5 x))) '(4 5 6))

(null? '())    ; => #t
(null? (list)) ; => #t
(null? '(a))   ; => #f
(null? 'a)     ; => #f

(pair? '())      ; => #f
(pair? '(a))     ; => #t
(pair? '(a b))   ; => #t
(pair? '(a . b)) ; => #t
(pair? 'a)       ; => #f

(define nonpair?
  (lambda (x) (not (pair? x))))
(nonpair? '(1 2))
(nonpair? '(1 . 2))
(nonpair? '())

(define atom?
  (lambda (x)
    (and (nonpair? x) (not (null? x)))))
(map atom? '((1 2) () atom))

"P.448"
(define nonzero?
  (lambda (x) (not (zero? x))))
(map nonzero? '(-1 0 1))

(letrec (
         (even? (lambda (x) (or  (zero?    x) (odd?  (- x 1)))))
         (odd?  (lambda (x) (and (nonzero? x) (even? (- x 1))))))
  (print (even? 3))
  (print (even? 4))
  "end")
