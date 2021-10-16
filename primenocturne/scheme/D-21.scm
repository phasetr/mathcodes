(load "./lib")
"P.814"
"D.21 合同計算と素数"
"D.21.1 合同式と根の個数"
(define (lce a b n)
  "linear congruence equation"
  (if (not (= 1 (gcd a n)))
      'not-coprime
      (/: (* b (** a (- (euler-phi n) 1))) n)))
(lce 26 1 13)
(lce 26 1 57)
(lce 13 1 7)
;;;(lce 5 1 7000000012680000000144)
(factorize-of 70000000144)
(euler-phi 70000000144)

"P.815"
(define (qce a b c p)
  "quadratic congruence equation"
  (let ((eq (lambda (x) (+ (* a x x) (* b x) c))))
    (let loop ((i 0) (tmp '()))
      (cond ((< (- p 1) i) (reverse tmp))
            ((= (/@ (eq i) p) 0)
             (loop (++ i) (cons i tmp)))
            (else (loop (++ i)    tmp))))))
(qce 3 -4 6  5)
(qce 2  3 1  7)
(qce 1  0 1 13)
(qce 1  0 1  7)

(qce 1 0 -1 7)
(qce 1 0 -1 2)
(qce 1 0 -1 4)
(qce 1 0 -1 8)

"P.816"
(define (B n)  (iota 1 n 2))
(define (Q1 n) (iota 1 n 4))
(define (O1 n) (iota 1 n 8))
(Q1 100)
(O1 100)
(define (Opm1 n)
  (sort (flatten (list (iota 1 n 8) (iota 7 n 8)))))
(Opm1 100)
(define (Ppm1 n)
  (filter prime? (Opm1 n)))
(Ppm1 100)
(define (P4n1 n)
  (filter (lambda (x) (if (= (/@ x 4) 1) #t #f))
          (primes n)))
(P4n1 100)

"P.817"
"D.21.2 原子根を求める"
(define (min-pr p)
  (define (pr x k p)
    (let ((p-1? (= (- p 1) k))
          (one? (= (/:** x k p) 1)))
      (cond ((and      p-1?  one?)  1)
            ((and (not p-1?) one?) -1)
            (else 0))))
  (let loop ((x 2) (k 1))
    (cond ((=  1 (pr x k p)) x)
          ((= -1 (pr x k p)) (loop (++ x) 1))
          (else              (loop x (++ k))))))
(map (lambda (x) (list x (min-pr x)))
     (cdr (primes 41)))

(define (p-root p)
  ;;;"primitive root"
  (define (co-p p)
    (let loop ((i 2) (tmp '()))
      (cond ((< (- p 2) i) (reverse tmp))
            ((= (gcd i (- p 1)) 1)
             (loop (++ i) (cons i tmp)))
            (else (loop (++ i)    tmp)))))
  (let ((min (min-pr p)))
    (sort (cons min
                (map (lambda (x) (/@ (** min x) p))
                     (co-p p))))))
"P.818"
(p-root 7)
(p-root 17)
(p-root 41)
"D.21.3 指数を求める"
(define (ind-pr p)
  (define (ind-cal p lst)
    (map (lambda (val)
           (list
            val
            (map
             (lambda (ind) (cadr ind))
             (map
              (lambda (num)
                (assoc num
                       (map
                        (lambda (x)
                          (list (/:** val x p) x))
                        (iota 0 (- p 2)))))
              (iota (- p 1)))))) lst))
  (let ((lst (p-root p)))
    (ind-cal p lst)))
(ind-pr 7)
(car (ind-pr 23))

"P.819"
"D.21.4"
(define (sum2sq p)
  (define (fid p x y k)
    ;;;"Fermat infinite descent"
    (let* ((shift (lambda (z m)
                    (if (> (/@ z m) (/ m 2))
                        (- (/@ z m) m)
                        (/@ z m))))
           (rx (shift x k))
           (ry (shift y k))
           (x1 (abs (/ (+ (* x rx) (* y ry)) k)))
           (y1 (abs (/ (- (* y rx) (* x ry)) k)))
           (k1 (/ (+ (** x1 2) (** y1 2)) p)))
      (if (= k1 1)
          (list p x1 y1)
          (fid  p x1 y1 k1))))
  (let* ((g (min-pr p))
           (x0 (/:** g (/ (- p 1) 4) p))
           (y0 1)
           (k0 (/ (+ (** x0 2) (** y0 2)) p)))
      (if (= k0 1)
          (list p x0 y0)
          (fid  p x0 y0 k0))))
(map sum2sq (P4n1 80))
