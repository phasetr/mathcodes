(load "./lib")
"P.804"
"D.20 オイラーの関数"
"D.20.1 式のコード化"
(define (euler-phi . num)
  (let ((f1 (lambda x (- 1 (/ 1 (car x)))))
        (f2 (lambda x (- (car x) 1)))
        (p1 (car num))
        (p2 (cdr num)))
    (if (= p1 1)
        1
        (if (null? p2)
            (apply * (cons p1 (map f1 (p-factor p1))))
            (apply * (map f2 num))))))
"P.805"
(define (p-factor num)
  (map (lambda lst (caar lst))
       (factorize-of num)))
(factorize-of 30)
(p-factor 30)
(euler-phi 30)
(euler-phi 2 3 5)

(define (totient n)
  "totient function"
  (apply +
         (map (lambda (x) (if (= 1 (gcd n x)) 1 0))
              (iota n))))
(totient 30)

"P.806"
(map (lambda (x) (euler-phi x)) (iota 100))
(save->data 'P806-euler-phi.dat
            (map (lambda (x) (euler-phi x)) (iota 100)))

"P.807"
(map (lambda (x) (euler-phi (** 10 x))) (iota 10))

"P.808"
"D.20.2 自然数の相互関係"
"オイラーの関数による方法"
(define (coprime2 n)
  (list (apply + (map euler-phi (iota n)))
        '/ (hc n 2)))
"P.809"
(coprime2 5)

(define (coprime2 n)
  (* 1.0
     (/ (apply + (map euler-phi (iota n)))
        (/ (* n (+ n 1)) 2))))
(coprime2 5)
(let ((pi (* 4.0 (atan 1.0))))
  (/ 6 (* pi pi)))

"P.810"
"格子点の直接探索"
(define (coprime3 n)
  (let* ((judge (lambda (x)
                  (if (= 1 (gcd (car x) (cadr x) (caddr x)))
                      1 +i))))
    (apply + (map judge (element-h n 3)))))
(coprime3 10)

"P.811"
(define (coprime3 n)
  (let* ((judge (lambda (x1 x2 x3)
                  (if (<= x1 x2 x3)
                      (if (= 1 (gcd x1 x2 x3)) 1 +i) 0)))
         (calc (lambda (z)
                 (/ (real-part z)
                    (+ (real-part z) (imag-part z))))))
    (let loop ((i 1) (j 1) (k 1) (tmp 0))
      (cond ((<= k n) (loop i j (++ k) (+ (judge i j k) tmp)))
            ((<  j n) (loop i (++ j) 1 tmp))
            ((<  i n) (loop (++ i) 1 1 tmp))
            (else (calc tmp))))))

"P.812"
"モンテカルロ法による探索"
(define (mc-coprime2 range k)
  (let loop ((i 0) (hits 0))
    (let ((x1 (random+ range))
          (x2 (random+ range)))
      (if (< i k)
          (if (= 1 (gcd x1 x2))
              (loop (++ i) (+ hits 1))
              (loop (++ i)    hits))
          (* 1.0 (/ hits k))))))
(mc-coprime2 10000 1000000)

(define (mc-coprime3 range k)
  (let loop ((i 0) (hits 0))
    (let ((x1 (random+ range))
          (x2 (random+ range))
          (x3 (random+ range)))
      (if (< i k)
          (if (= 1 (gcd x1 x2 x3))
              (loop (++ i) (+ hits 1))
              (loop (++ i)    hits))
          (* 1.0 (/ hits k))))))
(mc-coprime3 10000 1000000)

"P.813"
(define (mc-coprime4 range k)
  (let loop ((i 0) (hits 0))
    (let ((x1 (random+ range))
          (x2 (random+ range))
          (x3 (random+ range))
          (x4 (random+ range)))
      (if (< i k)
          (if (= 1 (gcd x1 x2 x3 x4))
              (loop (++ i) (+ hits 1))
              (loop (++ i)    hits))
          (* 1.0 (/ hits k))))))
(mc-coprime4 10000 1000000)

(define (phi-sum n)
  (apply +
         (map euler-phi (factors-of n))))
(phi-sum 23)
