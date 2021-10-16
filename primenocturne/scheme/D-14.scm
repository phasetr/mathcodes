(load "./lib")
"P.748"
"D.14 モンテカルロ法"
"D.14.1 面積を求める"
(define (mc-method pred k)
  (let loop ((i 0) (hits 0))
    (if (= i k)
        hits
        (loop (++ i)
              (+ hits
                 (if (pred (random-real)
                           (random-real))
                     1 0))))))

"P.749"
(define (eq-pi x y)
  (<= (+ (** x 2) (** y 2)) 1))
(output (map
         (lambda (x)
           (let* ((hitnum (mc-method eq-pi x))
                  (app-pi (/ (* 4 hitnum) x)))
             (list hitnum app-pi)))
         '(10 100 1000 10000 100000 1000000)))

(save->data 'P750-random-2d.dat
            (rand2d 4000))

"P.750"
"アステロイドの面積を求める"
"P.751"
(define (eq-ast x y)
  ;;; 0^{2/3} がエラーになるのでそれを除外
  (let ((x0 (if (< x m-eps) m-eps x))
        (y0 (if (< y m-eps) m-eps y)))
    (<= (+ (** x0 (/ 2 3))
           (** y0 (/ 2 3))) 1)))
(output (map
         (lambda (x)
           (let* ((hitnum (mc-method eq-ast x))
                  (app-ast (* 1.0 (/ hitnum x))))
             (list hitnum app-ast)))
         '(10 100 1000 10000)))
(/ (* 3 (* 4 (atan 1))) 32)

(save->data 'P751-random-2d.dat
            (mc-method eq-ast 1000))

"P.753"
(define (eq-lem x y)
  (<= (- (** (+ (** x 2) (** y 2)) 2)
         (- (** x 2) (** y 2))) 0))
(output (map
         (lambda (x)
           (let* ((hitnum (mc-method eq-lem x))
                  (app-lem (* 1.0 (/ hitnum x))))
             (list hitnum app-lem)))
         '(10 100 1000 10000 100000)))

"D.14.2 三次元への拡張"
"P.754"
(define (rand3d k)
  (map (lambda (x)
         (list (random-real) (random-real) (random-real)))
       (iota k)))
(save->data 'P754-random-3d.dat (rand3d 4000))

(define (mc-method3 pred k)
  (let loop ((i 0) (hits 0))
    (if (= i k)
        hits
        (loop (++ i)
              (+ hits (if (pred (random-real) (random-real) (random-real))
                          1 0))))))
(define (eq-pi3 x y z)
  (<= (+ (** x 2) (** y 2) (** z 2)) 1))

"P.755"
(output (map
         (lambda (x)
           (let* ((hitnum (mc-method3 eq-pi3 x))
                  (apprx (* 1.0 (/ hitnum x))))
             (list hitnum apprx)))
         '(10 100 1000 10000 100000 1000000)))

"P.756"
"D.14.3 ピタゴラス数を求める"
"述語を定める"
"P.757"
(define (Pythagorean? x y z)
  "全体が定数倍になっている自明な組は除くようにしてある: gcd=1? の部分"
  (and (< x y)
       (gcd=1? x y z)
       (= (+ (** x 2) (** y 2)) (** z 2))))
(Pythagorean? 3 4 5)
(Pythagorean? 6 8 10)

(define (gcd=1? x y z)
  (if (= (gcd x y z) 1) #t #f))
(gcd=1? 6 8 10)
(gcd=1? 3 4 5)

(define (diff? x lst)
  (not (member x lst)))
"境界上の疑似乱数"
(define (mc-xyz pred z)
  (let loop ((i 0) (tmp '()))
    (let ((x (random+ z)) (y (random+ z)))
      (cond ((< (* z z) i) tmp)
            ((and (pred x y z) (diff? (list x y z) tmp))
             (loop (++ i) (cons (list x y z) tmp)))
            (else (loop (++ i) tmp))))))
"P.758"
(define (mc-all pred max)
  (let loop ((i max) (tmp '()))
    (cond ((= 3 i) tmp)
          ((null? (mc-xyz pred i)) (loop (-- i) tmp))
          (else (loop (-- i)
                      (append (mc-xyz pred i) tmp))))))
(mc-all Pythagorean? 100)
"実験結果"
"P.759"
"リストによる方法"
(define (sqn-car? lst)
  (sqn? (car lst)))
(filter sqn-car? (triple 15))

(define (sqrt-car lst)
  (cons (inexact->exact (sqrt (car lst)))
        (cdr lst)))
(map sqrt-car (filter sqn-car? (triple 15)))

(define (gcd-xyz? lst)
  (if (= (gcd (car lst) (cadr lst) (cadr lst)) 1)
      #t #f))

(filter gcd-xyz?
        (map sqrt-car (filter sqn-car? (triple 15))))

"P.760"
(define (triple-xyz n)
  (reverse
   (reverse-all
    (filter gcd-xyz?
            (map sqrt-car
                 (filter sqn-car?
                         (triple n)))))))
(triple-xyz 15)

"D.14.4 ガウス素数の抽出"
(define (prime-car? n)
  (prime? (car n)))
(filter prime-car? (triple 5))

(define (p4n1? n)
  (if (and (prime? n) (= (/@ n 4) 1)) #t #f))
(define (p4n3? n)
  (if (and (prime? n) (= (/@ n 4) 3)) #t #f))
(filter p4n1? (iota 99))
(filter p4n3? (iota 99))

"P.761"
(define (sqsum-p lst)
  (remove null?
          (map (lambda (x) (if (prime-car? x) (cdr x) '())) lst)))
(sqsum-p (triple 5))

(define (quad1 lst)
  "quadruple"
  (let ((ones '((1 1) (1 -1) (-1 1) (-1 -1))))
    (map (lambda (x) (map * lst x)) ones)))
(define (quad2 lst)
  "quadruple"
  (let ((ones '((1 0) (-1 0) (0 1) (0 -1))))
    (map (lambda (x) (map * lst x)) ones)))
(quad1 '(2 3))
(quad2 '(2 3))

(define (gauss-p1 lst)
  (let* ((ex (lambda (l) (list (cadr l) (car l)))))
    (append (quad1 lst) (quad1 (ex lst)))))
(flat-in (map gauss-p1 (sqsum-p (triple 5))))

"P.762"
(define (gauss-p2 n)
  (let ((lst (list n n)))
    (quad2 lst)))
(flat-in (map gauss-p2 (filter p4n3? (iota 25))))

(define gauss-p3 (quad1 '(1 1)))
;;;(define gauss-p0 (gauss-p2-test 1))
(define gauss-p0 (gauss-p2 1))

(define (gauss-p n)
  (flat-in
   (append
    (list gauss-p0 '(()) gauss-p3)
    (map gauss-p1 (sqsum-p (triple n)))
    (map gauss-p2 (filter p4n3? (iota n))))))
(define (diskcut k)
  (filter list?
          (map (lambda (x)
                 (cond ((equal? x '()) '())
                       ((<= (sq+ (car x) (cadr x)) (** k 2)) x)
                       (else 'del-term)))
               (gauss-p k))))
"P.763"
(save->data 'P763-gauss50.dat (diskcut 50))
