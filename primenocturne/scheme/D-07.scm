(load "./lib")
"P.679"
"D.7 連分数・円周率・ベルヌーイ数"
"．７.1 連分数による無理数の定義"
(define (cont-frac para-func)
  (define (con i)
    (if (= i 25)
        0
        (/ 1 (+ (para-func i) (con (++ i))))))
  (+ (para-func 0) (con 1)))
(define (golden-num n) 1.0)
(cont-frac golden-num)

"平方根を求める"
(define (sq2 n)
  (cond ((= n 0) 1.0)
        (else 2)))
(cont-frac sq2)

(define (sq5 n)
  (cond ((= n 0) 2.0)
        (else 4)))
(cont-frac sq5)

(define (sq10 n)
  (cond ((= n 0) 3.0)
        (else 6)))
(cont-frac sq10)

(define (sq17 n)
  (cond ((= n 0) 4.0)
        (else 8)))
(cont-frac sq17)

(define (sq3 n)
  (cond ((= n 0) 1.0)
        ((= (/@ n 2) 1) 1)
        ((= (/@ n 2) 0) 2)))
(cont-frac sq3)

(define (sq6 n)
  (cond ((= n 0) 2.0)
        ((= (/@ n 2) 1) 2)
        ((= (/@ n 2) 0) 4)))
(cont-frac sq6)

(define (sq8 n)
  (cond ((= n 0) 2.0)
        ((= (/@ n 2) 1) 1)
        ((= (/@ n 2) 0) 4)))
(cont-frac sq8)

(define (sq11 n)
  (cond ((= n 0) 3.0)
        ((= (/@ n 2) 1) 3)
        ((= (/@ n 2) 0) 6)))
(cont-frac sq11)

(define (sq12 n)
  (cond ((= n 0) 3.0)
        ((= (/@ n 2) 1) 2)
        ((= (/@ n 2) 0) 6)))
(cont-frac sq12)

(define (sq15 n)
  (cond ((= n 0) 3.0)
        ((= (/@ n 2) 1) 1)
        ((= (/@ n 2) 0) 6)))
(cont-frac sq15)

(define (sq18 n)
  (cond ((= n 0) 4.0)
        ((= (/@ n 2) 1) 4)
        ((= (/@ n 2) 0) 8)))
(cont-frac sq18)

(define (sq20 n)
  (cond ((= n 0) 4.0)
        ((= (/@ n 2) 1) 2)
        ((= (/@ n 2) 0) 8)))
(cont-frac sq20)

"P.681"
(define (sq7 n)
  (cond ((= n 0) 2.0)
        ((= (/@ n 4) 1) 1)
        ((= (/@ n 4) 2) 1)
        ((= (/@ n 4) 3) 1)
        ((= (/@ n 4) 0) 4)))
(cont-frac sq7)

(define (sq14 n)
  (cond ((= n 0) 3.0)
        ((= (/@ n 4) 1) 1)
        ((= (/@ n 4) 2) 2)
        ((= (/@ n 4) 3) 1)
        ((= (/@ n 4) 0) 6)))
(cont-frac sq14)

(define (sq13 n)
  (cond ((= n 0) 3.0)
        ((= (/@ n 5) 1) 1)
        ((= (/@ n 5) 2) 1)
        ((= (/@ n 5) 3) 1)
        ((= (/@ n 5) 4) 1)
        ((= (/@ n 5) 0) 6)))
(cont-frac sq13)

(define (sq19 n)
  (cond ((= n 0) 4.0)
        ((= (/@ n 6) 1) 2)
        ((= (/@ n 6) 2) 1)
        ((= (/@ n 6) 3) 3)
        ((= (/@ n 6) 4) 1)
        ((= (/@ n 6) 5) 2)
        ((= (/@ n 6) 0) 8)))
(cont-frac sq19)

"円周率を求める"
(define (cont-frac para-a para-b)
  (define (con i)
    (if (= i 25)
        0
        (/ (para-a i) (+ (para-b i) (con (++ i))))))
  (+ (para-b 0) (con 1)))
(define (pi-a n)
  (cond ((= n 1) 4.0)
        (else (** (- n 1) 2))))
(define (pi-b n)
  (cond ((= n 0) 0)
        (else (- (* 2 n) 1))))
"P.682"
(cont-frac pi-a pi-b)

"正多角形と円周率"
(define (pi-gon n)
  (define (gon n)
    (if (zero? n)
        1
        (sqrt (- 2 (sqrt (- 4 (expt (gon (-- n)) 2)))))))
  (* 3 (** 2 n) (gon n)))
(map pi-gon (iota 0 7))

"D.7.2 ベルヌーイ数とゼータ関数"
(define bernoulli
  (memoize
   (lambda (n)
     (let ((fn (lambda (x)
                 (* (/ -1 (+ n 1)) (comb (+ n 1) x)))))
       (let loop ((k 0) (num 0))
         (cond ((= n 0) 1)
               ((< (- n 1) k) num)
               (else (loop (++ k)
                           (+ (* (fn k) (bernoulli k)) num)))))))))
(map bernoulli (iota 0 14))

(define (b-poly n x)
  "bernoulli polynomials"
  (apply + (map (lambda (k)
                  (* (comb n k) (bernoulli k) (** x (- n k))))
                (iota 0 n))))
"P.684"
(define (faulhaber i n)
  (* (/ 1 (+ i 1))
     (- (b-poly (+ i 1) (+ n 1))
        (b-poly (+ i 1) 1))))
(map (lambda (x) (faulhaber x 10)) (iota 0 9))

(define (decomp k)
  (let* ((b-num (remove zero?
                        (map bernoulli (iota 0 k))))
         (fn (lambda (x)
               (list (numerator x)
                     (factorize-of (denominator x))))))
    (map fn b-num)))
(output (decomp 50))

(define (zeta2k k)
  (let* ((c (/ (* (** 2 (- k 1)) (abs (bernoulli k)))
               (fact k)))
         (n (numerator c))
         (d (denominator c))
         (z (* c (** pi k))))
    (cond ((odd? k) 'error)
          ((= n 1) (list   'pi^ k '/ d '= z))
          (else    (list n 'pi^ k '/ d '= z)))))
(map zeta2k (iota 2 14 2))
(zeta2k  2)
(zeta2k  4)
(zeta2k  6)
(zeta2k  8)
(zeta2k 10)
(zeta2k 12)
(zeta2k 14)
