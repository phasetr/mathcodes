(load "./lib")
"P.741"
"D.13 疑似乱数を作る"
"P.742"
"D.13.1 線型合同法"
"合同式に始まる"
"P.743"
"三条件を定める"
"P.744"
(gcd 1013904223 (** 2 32))
(gcd 1664524 (** 2 32))

(define (gene32 seed)
  (let ((x seed))
    (lambda ()
      (let ((a 1664525) (c 1013904223) (m (** 2 32)))
        (set! x (/: (+ (* a x) c) m))))))
(define random32 (gene32 1))
(map (lambda (x) (random32)) (iota 0 10))

"D.13.2 規則性の切除"
"P.745"
(map (lambda (x) (/: (random32) 2)) (iota 1 20))
(map (lambda (x) (/: (random32) 4)) (iota 1 20))
(map (lambda (x) (/: (random32) 8)) (iota 1 20))

"下位桁を捨てる"
(map (lambda (x) (// (random32) (** 2 16))) (iota 1 20))

(define (gene16 seed type)
  (let ((x seed))
    (lambda ()
      (let ((a 1664525)
            (c 1013904223)
            (m (** 2 32))
            (k (** 2 16)))
        (set! x (/: (+ (* a x) c) m))
        (* (** (/ 1 k) type) (// x k))))))
"P.746"
(define random-int  (gene16 1 0))
(define random-frac (gene16 1 1))
(define random-real (gene16 1 1.0))
(map (lambda (x) (random-int)) (iota 1 20))
(map (lambda (x) (random-frac)) (iota 1 20))
(map (lambda (x) (random-real)) (iota 1 20))

(define (random n) (/: (random-int) n))
(define (random+ n) (+ 1 (/: (random-int) n)))

"D.13.3 実行と検証"

(map (lambda (x) (random 2)) (iota 1 20))
(define random-int-seed2 (gene16 2 0))
(map (lambda (n) (/: (random-int-seed2) 2)) (iota 1 20))
(map (lambda (n) (/: (random-int-seed2) 4)) (iota 1 20))

"P.747"
"TODO P.747 上の左側の図"
(define (rand2d k)
  (map (lambda (x) (list (random-real) (random-real)))
       (iota k)))
(save->data 'P747-random-2d.dat (rand2d 1000))

(save->data 'P747-mc-colors.dat
            (repdec (lambda (x) (random 10))
                    2500))
