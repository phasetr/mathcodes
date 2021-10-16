(load "./lib")
"P.784"
"D.18 無限ストリーム"
"遅延リスト"
"P.785"
(define-syntax s-cons
  (syntax-rules ()
    ((_ x y) (cons x (delay y)))))

(define (int-from n)
  (s-cons n (int-from (+ n 1))))

(define numbers* (int-from 1))
(define (s-car stm)        (car stm))
(define (s-cdr stm) (force (cdr stm)))

(s-car numbers*)
(s-cdr numbers*)
(s-cdr (s-cdr numbers*))

"P.786"
(s-cdr (s-cdr (s-cdr numbers*)))

"各種手続の更新"
(define (s-ref stm n)
  (if (zero? n)
      (s-car stm)
      (s-ref (s-cdr stm) (- n 1))))
(s-ref numbers* 4)
(s-ref numbers* 365)

"P.787"
(define s-head
  (lambda (stm n)
    (if (zero? n)
        '()
        (cons (s-car stm)
              (s-head (s-cdr stm) (- n 1))))))
(s-head numbers* 10)

(define (s-iota min max)
  (if (< max min)
      '()
      (s-head (int-from min) (+ (- max min) 1))))
(s-iota -5 15)

(define (s-filter pred stm)
  (cond ((s-null? stm) '())
        ((pred (s-car stm))
         (s-cons (s-car stm)
                 (s-filter pred (s-cdr stm))))
        (else (s-filter pred (s-cdr stm)))))
(define s-null? null?)

"P.788"
(define (s-remove pred stm)
  (cond ((null? stm) '())
        ((pred (s-car stm))
         (s-remove pred (s-cdr stm)))
        (else (s-cons (s-car stm)
                      (s-remove pred (s-cdr stm))))))
(s-head (s-filter even? numbers*) 10)
(s-head (s-remove even? numbers*) 10)

"擬似素数を求める"
(define (p-prime? x)
  (and (zero? (/@ x 2)) (fermat? 2 x)))

(s-ref (s-filter p-prime? numbers*) 0)
(factorize-of 161038)

"P.789"
(s-ref (s-filter p-prime? numbers*) 1)
(factorize-of 215326)

(factorize-of 2568226)

"P.790"
"素数とフィボナッチのストリーム"
(define (/@zero? x y)
  (= (/@ x y) 0))
(s-head
 (s-remove
  (lambda (x) (/@zero? x 2)) numbers*)
 10)

(define (sieve* stm)
  (s-cons
   (s-car stm)
   (sieve* (s-remove
            (lambda (x) (/@zero? x (s-car stm)))
            (s-cdr stm)))))
(define primes* (sieve* (int-from 2)))
(s-head primes* 25)
(define (p-nth n)
  (s-ref primes* (- n 1)))
(p-nth 1)
(p-nth 10)
(p-nth 100)

"P.791"
(p-nth 1000)
(define (perfect? n)
  (= (- (sigma-of n) n ) n))
(define perfect-numbers*
  (s-filter perfect? numbers*))
(s-head perfect-numbers* 3)

(define (abundant? n)
  (< n (- (sigma-of n) n)))
(define abundant-number*
  (s-filter abundant? numbers*))
(s-head abundant-number* 22)

(s-head (s-filter odd? abundant-number*) 12)
(factors-of 945)

"P.792"
(factorize-of 945)

(define (fib-gen a b)
  (s-cons a (fib-gen b (+ a b))))
(define fibseq* (fib-gen 1 1))
(s-head fibseq* 20)
(s-ref fibseq* 99)

"単項目のストリームを作る"
(define ones* (s-cons 1 ones*))
(s-head ones* 10)
(define minusls* (s-cons -1 minusls*))
(define zeros*   (s-cons  0 zeros*))

"P.793"
(s-head minusls* 10)
(s-head zeros* 10)
(define sign*
  (s-cons 1 (s-cons -1 sign*)))
(s-head sign* 10)

"ストリームの加算・乗算"
(define (s-add stm1 stm2) (s-map + stm1 stm2))
(define (s-mul stm1 stm2) (s-map * stm1 stm2))

(define map-strict
  (lambda (proc . rest)
    (define map-unit
      (lambda (proc lst)
        (if (null? lst)
            '()
            (cons (proc (car lst))
                  (map-unit proc (cdr lst))))))
    (if (null? (car rest))
        '()
        (cons (apply            proc (map-unit car rest))
              (apply map-strict proc (map-unit cdr rest))))))

"P.794"
(define (map-strict proc . rest)
  (if (null? (car rest))
      '()
      (cons (apply            proc (map car rest))
            (apply map-strict proc (map cdr rest)))))
(define (s-map proc . rest)
  (if (null? (car rest))
      '()
      (s-cons (apply       proc (map s-car rest))
              (apply s-map proc (map s-cdr rest)))))

(s-map even? '(1 2 3))
(s-cdr (s-map even? '(1 2 3)))
(s-map * '(2 3 5) '(7 11 13) '(17))
(s-cdr (s-map * '(2 3 5) '(7 11 13) '(17 19)))
(s-cdr (s-cdr (s-map * '(2 3 5) '(7 11 13) '(17 19 23))))

(s-head (s-add numbers* minusls*) 10)
(s-head (s-add numbers* numbers*) 10)

"P.795"
(s-head (s-cdr numbers*) 10)
(s-head (s-cdr (s-cdr numbers*)) 10)

(define fibs*
  (s-cons 1
          (s-cons 1
                  (s-add fibs* (s-cdr fibs*)))))
(s-head (s-mul numbers* numbers*) 10)
(s-head (s-mul numbers* (s-mul numbers* numbers*)) 10)

(define fact*
  (s-cons 1
          (s-mul fact* (s-cdr numbers*))))
(s-head fact* 10)

"ストリームによる数値計算"
(define (sum-list stm)
  (define sum-gen
    (s-cons (s-car stm)
            (s-add (s-cdr stm) sum-gen)))
  sum-gen)
"P.796"
(s-head
 (sum-list
  (cons 1.0
        (s-map (lambda (x) (/ 1 x)) fact*))) 5)

(define (s-mag stm coef)
  (s-map (lambda (x) (* x coef)) stm))
(define times2*
  (s-cons 1 (s-mag times2* 2)))
(s-head times2* 10)

(define half*
  (s-cons 1 (s-mag half* 1/2)))
(s-head half* 10)

(define inv*
  (s-map (lambda (x) (/ 1 x)) numbers*))
(s-head inv* 10)

"P.797"
(s-head (s-mul inv* sign*) 10)
(* 1.0 (apply + (s-head (s-mul inv* sign*) 100)))

(define inv-odd*
  (s-map
   (lambda (x) (/ 1 x))
   (s-filter odd? numbers*)))
(s-head inv-odd* 10)
(* 4.0 (apply + (s-head (s-mul inv-odd* sign*) 100)))

"無限を加える"
(define (s-append stm1 stm2)
  (if (s-null? stm1)
      stm2
      (s-cons (s-car stm1)
              (s-append (s-cdr stm1) stm2))))
"P.798"
(s-head (s-append ones* minusls*) 10)
(s-head (s-append
         (s-filter odd?  numbers*)
         (s-filter even? nu)) 10)

(define (interleave stm1 stm2)
  (if (s-null? stm1)
      stm2
      (s-cons (s-car stm1)
              (interleave stm2 (s-cdr stm1)))))
(s-head (interleave ones* minusls*) 10)
(s-head
 (interleave
  (s-filter odd?  numbers*)
  (s-filter even? numbers*)) 10)
