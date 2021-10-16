(load "./lib")

"P.584"
"D.1 組込関数による数値計算"
(floor 3)
(floor 3.14)
(floor -2.72)

(ceiling 3)
(ceiling 3.14)
(ceiling -2.72)

(define (room n)
  (list 'ceiling= (ceiling n) 'floor= (floor n)))
(room 3)
(room 3.14)
(room -2.72)

"P.585"
(define pi 3.141592653589793)
(sin (/ pi 3))
(cos (/ pi 3))
(tan (/ pi 3))
(* 3 (asin (sin (/ pi 3))))
(* 3 (acos (cos (/ pi 3))))
(* 3 (atan (tan (/ pi 3))))

(exp 0)
(exp 1)
(log 1)
(log 10)

(log 2)
(log 3)
(log 10)
(log (exp 1))

(/ (log 2) (log 10))
(/ (log 3) (log 10))
(define (log10 x)
  (/ (log x) (log 10)))
(log10 2)
(log10 3)

(define (log2 x)
  (/ (log x) (log 2)))
(log2 2)
(log2 3)

"素数定理: pnt, prime number theorem"
(define (pnt n)
  (adjust-of (/ n (log n))))
(map pnt (map (lambda (x) (** 10 x)) (iota 10)))

;;;(define ((compose f g) x)
;;;  (f (g x)))
"P.586"
((compose log exp) 1)
((compose exp log) 1)
((compose sin asin) 1)
((compose asin sin) 1)
((compose / /) 1)
((compose - -) 1)
((compose not not) #t)

"虚数単位: +i"
(expt +i +i)
(exp (/ pi -2))

"オイラーの公式"
(exp (* +i pi))

"P.587"
(define (adjust-i z)
  (let ((a (adjust-of (real-part z)))
        (b (adjust-of (imag-part z))))
    (make-rectangular a b)))
(adjust-i (exp (* +i pi)))

"1のn乗根"
(define (root-unity n)
  (map (lambda (k) (adjust-i (exp (/ (* +i 2 pi k) n))))
       (iota 0 (- n 1))))
(root-unity 1)
(root-unity 2)
(root-unity 3)
(root-unity 4)
(root-unity 5)
(root-unity 6)
(root-unity 7)
(root-unity 8)

"極座標変換"
(define (rect->p z)
  (let* ((deg (/ 180 pi))
         (r (inexact->exact (adjust-of (magnitude z))))
         (az (angle z))
         (t (adjust-of (* deg
                          (if (negative? az) (+ az (* 2 pi)) az)))))
    (list r t)))

"P.588"
(define (root-unity+ n)
  (map (lambda (k) (rect->p (exp (/ (* +i 2 pi k) n))))
       (iota 0 (- n 1))))
(root-unity+ 1)
(map root-unity+ (iota 1 8))

"平方根"
(sqrt +i)
(exp (/ (* +i pi) 4))

(define (dec10 lst)
  (let ((modify (lambda (x) (adjust-of (* 1.0 x)))))
    (if (list? (car lst))
        (dismap modify lst)
        (map    modify lst))))
"P.589"
(dec10 '(1.0 0.5 0.33333 0.25 0.2 0.16666 0.142857))
(dec10 (map (compose (lambda (x) (* 1.0 x)) /) (iota 1 7)))
(dec10 '((1.0 1.0) (1.41421 0.5) (1.73205 0.33333)))
