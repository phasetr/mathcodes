(load "./lib")
"P.612"
"D.3 四則計算の仕組"
(define (// a b) (quotient  a b))
(define (/@ a b) (remainder a b))
(define (/: a b) (modulo    a b))

(// 7 3)
(/@ 7 3)
(/: 7 3)

(/@  7  3)
(/@ -7  3)
(/@  7 -3)
(/@ -7 -3)

(/:  7  3)
(/: -7  3)
(/:  7 -3)
(/: -7 -3)

"D.3.1 互除法"
(define (form-d a b)
  (list a '= (// a b) '* b '+ (/@ a b)))
"P.613"
(form-d 7 3)

(define (form-m a b)
  (list a 'equiv (/: (/@ a b) b) 'mod b))
(form-m 7 3)

(form-d 195 143)
(form-d 143  52)
(form-d  52  39)
(form-d  39  13)

(define (my-gcd a b)
  (if (zero? b)
      a
      (my-gcd b (/@ a b))))
"P.614"
(my-gcd 195 143)

"gcd, lcm は Scheme 組み込みで存在"
(gcd 195 143)
(lcm 195 143)
(gcd 11111 99999 123454321)
(lcm 11 1111 11111)

"D.3.2 倍数と約数"
(define (my-even? x) (= (/@ x 2) 0))
(define (my-odd   x) (= (/@ x 2) 1))

"自然数の分割"
(define (R3-0? x) (= (/@ x 3) 0))
(define (R3-1? x) (= (/@ x 3) 1))
(define (R3-2? x) (= (/@ x 3) 2))

(define num60 (iota 1 60))
(filter R3-0? num60)
"P.615"
(filter R3-1? num60)
(filter R3-2? num60)

(define (R5-0? x) (= (/@ x 5) 0))
(define (R7-0? x) (= (/@ x 7) 0))

(filter R5-0? num60)
(filter R7-0? num60)
(remove R5-0? num60)
(remove R7-0? num60)

"約数を求める"
(define (factors-of n)
  "処理速度は遅いので注意"
  (let loop ((i n) (lst '()))
    (cond ((zero? i) lst)
          ((/@zero? n i) (loop (-- i) (cons i lst)))
          (else          (loop (-- i)         lst)))))
(define (/@zero? x y)
  (zero? (/@ x y)))
(factors-of 60)

(define (sigma-of n)
  (apply + (factors-of n)))
"P.616"
(- (sigma-of 60) 60)

(define (perfect? n)
  (= (- (sigma-of n) n) n))
(perfect? 6)
(perfect? 28)

"素因数分解"
(define (minid-of n)
  (define (next k)
    (if (= k 2) 3 (+ k 2)))
  "minimum divisor"
  (let ((i 2))
    (let loop ((i i))
      (cond ((< n (sq i))  n)
            ((/@zero? n i) i)
            (else (loop (next i)))))))

(define (prime? n)
  (if (= n 1)
      #f
      (= n (minid-of n))))

(define (factp-of n)
  "factors of prime number"
  (let loop ((i n) (tmp '()))
    (let ((div (minid-of i)))
      (if (= i div)
          (reverse (cons i tmp))
          (loop (/ i div) (cons div tmp))))))
(factp-of 75600)

(define (wall lst)
  (define pivot?
    (lambda (x) (lambda (y) (= x y))))
  (let loop ((lst1 '()) (lst2 lst))
    (if (null? lst2)
        (reverse lst1)
        (loop
         (cons (filter (pivot? (car lst2)) lst2) lst1)
         (remove (pivot? (car lst2)) lst2)))))
(wall '(2 2 2 2 3 3 3 5 5 7))

"P.618"
(define (fact-exp lst)
  (let loop ((lst1 '()) (lst2 lst))
    (if (null? lst2)
        (reverse lst1)
        (loop
         (cons (list (caar lst2) (length (car lst2)))
               lst1)
         (cdr lst2)))))
(fact-exp (wall '(2 2 2 2 3 3 3 5 5 7)))
(define (factorize-of n)
  ((compose fact-exp wall factp-of) n))
(factorize-of 75600)

"約数の個数"
"P.619"
(define (tau-of n)
  (define (fact-list lst)
    (if (null? lst)
        '()
        (cons (+ (cadar lst) 1)
              (fact-list (cdr lst)))))
  (if (= n 1)
      1
      (apply * ((compose fact-list factorize-of) n))))
(tau-of 75600)

(define (cn-tau k)
  "tau for composit number"
  (let loop ((i 1))
    (if (= k (tau-of i))
        i
        (loop (++ i)))))
(cn-tau 100)

"平方数を求める"
(define (sqn? n)
  "square number"
  (if (odd? (tau-of n))
      #t
      #f))
(filter sqn? (iota 400))

"P.620"
(map (lambda (x) (inexact->exact (sqrt x)))
     (filter sqn? (iota 400)))

"高度合成数"
(define (tauseq-to n)
  "tau sequence"
  (let loop ((k 1) (lst1 '()))
    (if (< n k)
        (reverse lst1)
        (loop (+ k 1) (cons (tau-of k) lst1)))))
(tauseq-to 24)

(define (tauseq-to-pair n)
  "tau sequence, もとの数が何か見やすくした"
  (let loop ((k 1) (lst1 '()))
    (if (< n k)
        lst1
        (loop (+ k 1) (cons (list k (tau-of k)) lst1)))))
(tauseq-to-pair 24)

"P.621"
(define (hcn-to n)
  (define data (tauseq-to n))
  "highly composite number"
  (let loop ((k 2) (lst1 '((1 1))))
    (if (< n k)
        (reverse lst1)
        (loop (+ k 1)
              (if (< (apply max (list-head data (- k 1)))
                     (tau-of k))
                  (cons (list k (tau-of k)) lst1)
                  lst1)))))
(hcn-to 24)

;;;(hcn-to 50000) ; 時間がかかる
(factorize-of 45360)

;(define hcn-data (hcn-to 50000))
;(assoc 720 hcn-data)

"P.622"
"親和数"
(define (sumpd-of n)
  "sum of proper divisors"
  (- (sigma-of n) n))
(sumpd-of 220)
(sumpd-of 284)
((compose sumpd-of sumpd-of) 220)

(define (aminum x)
  "amicable numbers"
  (let loop ((i 1) (tmp '()))
    (let* ((n (sumpd-of i)) (m (sumpd-of n)))
      (if (< x i)
          (reverse tmp)
          (if (and (= m i) (< m n))
              (loop (++ i) (cons (list m n) tmp))
              (loop (++ i) tmp))))))
(aminum 5000)

"フェルマー数"
(define (fermat-number n)
  (+ (** 2 (** 2 n)) 1))
"P.623"
(map (compose prime? fermat-number) (iota 1 4))
(map (compose prime? fermat-number) (iota 1 5))
(factorize-of (fermat-number 5))
(factorize-of (fermat-number 6))

"D.3.3 循環小数を計算する"
(define (/@check a b i)
  (if (zero? i)
      (/@ a b)
      (/@ (* 10 (/@check a b (-- i))) b)))
"P.624"
(map (lambda (x) (/@check 1 7 x)) (iota 0 7))

"計算の方針"
(define (//check a b i)
  (if (zero? i)
      (// a b)
      (// (* 10 (/@check a b (-- i))) b)))
(map (lambda (x) (//check 1 7 x)) (iota 0 7))

"P.625"
(define (member? x lst)
  (cond ((null? lst) #f)
        ((= (car lst) x) #t)
        (else (member? x (cdr lst)))))

(define (/// a b)
  (let loop ((i 0) (tmp1 '()) (tmp2 '()))
    (if (or (zero? (/@check a b i))
            (member? (/@check a b i) tmp1))
        (list (length tmp1)
              (reverse (cons (//check a b i) tmp2)))
        (loop (++ i)
              (cons (/@check a b i) tmp1)
              (cons (//check a b i) tmp2)))))

"巡回数"
(/// 1 7)
(/// 1 17)
(/// 1 19)
(/// 1 23)
(/// 1 29)

"P.626"
(/// 1 13)
(/// 1 103)
(/// 1 11)
(/// 1 37)

(/ 11 7)
(/ 11 7.0)
(// 11 7)
(/// 11 7)
(/@ 11 7)
(/: 11 7)

"D.3.4 循環小数を表記する"
"数を数字へ"
"P.627"
(define ten (iota 0 9))
(map number->string ten)
(apply string-append (map number->string ten))

(define (digit->string lst)
  (apply string-append
         (map number->string lst)))
(caadr (/// 1 7))
(cdadr (/// 1 7))
(number->string (caadr (/// 1 7)))
(digit->string  (cdadr (/// 1 7)))

"数字から数へ"
(define (decimal lst)
  (string-append
   (number->string (caadr lst)) "."
   (digit->string (cdadr lst))))
(decimal (/// 1 7))
"P.628"
(string->number (decimal (/// 1 7)))
(* 2 (string->number (decimal (/// 1 7))))
(decimal (/// 1 97))
(string->number (decimal (/// 1 97)))
(/ 1.0 97)
