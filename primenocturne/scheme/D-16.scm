(load "./lib")
"P.770"
"D.16 乱択アルゴリズム"
(primes 99)

"D.16.1 フェルマー・テスト"
(define (fermat? a p)
  (= (/: (** a p) p) a))
(map (lambda (n) (fermat? n 5)) (iota 1 4))
(map (lambda (n) (fermat? n 6)) (iota 1 5))

"P.772"
"冪計算の改善"
(define (/:** a p m)
  (cond ((zero? p) 1)
        ((even? p) (/: (sq  (/:** a (/ p 2) m)) m))
        ((odd?  p) (/: (* a (/:** a (- p 1) m)) m))))
(define (fermat? a p)
  (= (/:** a p p) a))

"P.773"
(number->string 43  2)
(number->string 43 16)
(number->string 43 10)
(number->string 43)

(define (base2 x)
  (define (string-divide s)
    (map string (string->list s)))
  (define (decompo lst)
    (cond ((not (list? lst)) (list (string->number lst)))
          ((null? lst) '())
          (else (cons (string->number (car lst))
                      (decompo (cdr lst))))))
  (let ((lst (string-divide (number->string x 2))))
  ;;(let ((lst (string-divide 1 (number->string x 2))))
    (decompo lst)))
(base2 43)
(base2 63)

(define (bit-and lst1 lst2)
  (apply * (remove zero? (map * lst1 lst2))))

"P.774"
(bit-and '(1 0 1 0 1 1) '(1 1 16 81 9 82))
(/: 11808 85)

(define (bit** y s n)
  (let ((bexp (base2 s)))
    (let loop ((k 0) (tmp '()) (t (/@ y n)))
      (if (= k (length bexp))
          (if (< n (bit-and bexp tmp))
              (/@  (bit-and bexp tmp) n)
              (bit-and bexp tmp))
          (loop (+ k 1) (cons t tmp) (/@ (* t t) n))))))

"P.775"
(bit** 82 43 85)
(bit** 2 1092 (** 1093 2))
(bit** 2 3510 (** 3511 2))

"証人の存在確率"
(define (witness k)
  (let loop ((i 2) (w 0))
    (cond ((= k i) w)
          ((eqv? (fermat? i k) #f)
           (loop (++ i) (++ w)))
          (else (loop (++ i) w)))))
(witness 5)
(witness 6)

(define (wit-list n)
  (let loop ((i 2) (tmp '()))
    (if (< n i)
        (reverse tmp)
        (loop (++ i) (cons (list i (witness i)) tmp)))))
(wit-list 20)

(define (wit-list-graph n)
  (let loop ((i 2) (tmp '()))
    (if (< n i)
        (reverse tmp)
        (loop (++ i) (cons (* 1.0 (/ (witness i) i)) tmp)))))
(save->data 'P775-wit-list.dat (wit-list-graph 100))

"P.776"
"D.16.2 カーマイケル数"
(define (wit-list n)
  (let loop ((i 2) (tmp '()))
    (if (< n i)
        (reverse tmp)
        (loop (++ i) (cons (witness i) tmp)))))
(define (unit x) (if (nonzero? x) (/ x x) x))
(define (exchange x) (if (zero? x) 1 0))

"P.777"
(map exchange (map unit (wit-list 20)))
(map unit (sieve 20))
(equal?
 (map exchange (map unit (wit-list 20)))
 (map unit (sieve 20)))

(define (flag n)
  (apply + (map -
                (map exchange
                     (map unit (wit-list n)))
                (map unit (sieve n)))))
(flag 20)
(flag 560)
(flag 561)

(flag 1104)
(flag 1105)

"P.778"
"素数である確率"
(define (random-test p)
  (let ((a (random+ (- p 1))))
    (= (/: (** a p) p) a)))

(define (judge? p i)
  (cond ((zero? i) #t)
        ((random-test p) (judge? p (-- i)))
        (else #f)))

(judge? 93 6)
(judge? 97 6)
"P.779"
(witness 91)
