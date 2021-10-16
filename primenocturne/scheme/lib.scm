(define m-eps (* 1.0 (expt 2 -24)))
(define pi/2 (asin 1))
(define pi (* 2 (asin 1)))
(define 3/2pi (* 3 pi/2))
(define 2pi (* 4 (asin 1)))
(define -2pi (- 2pi))
(define nonpair?
  (lambda (x) (not (pair? x))))

(define add1 (lambda (n) (+ n 1)))
(define ** (lambda (a b) (expt a b)))
(define ^ (lambda (a b) (expt a b)))
(define ++ (lambda (i) (+ i 1)))
(define -- (lambda (i) (- i 1)))

"distribution map"
(define dismap
  (lambda (proc dlst)
    (map (lambda (x) (map proc x)) dlst)))

(define adjust-of
  (lambda (x)
    (let ((digit 1000)
          (slide (if (positive? x) 1/2 -1/2)))
      (/ (truncate (+ slide (* digit x))) digit))))

(define iota
  (lambda lst
    (let* ((x (length lst))
           (max  (if (= 1 x) (car   lst) (cadr lst)))
           (min  (if (< 1 x) (car   lst) 1))
           (step (if (= 3 x) (caddr lst) 1)))
      (let loop ((i (- min step)) (tmp '()))
        (if (< (- max step) i)
            (reverse tmp)
            (loop (+ i step)
                  (cons (adjust-of (+ i step)) tmp)))))))

(define flatten
  (lambda (lst)
    (cond ((null? lst) '())
          ((pair? lst)
           (append (flatten (car lst))
                   (flatten (cdr lst))))
          (else (list lst)))))

(define sq  (lambda (x) (* x x)))

(define list-head
  (lambda (lst n)
    (if (zero? n)
        '()
        (cons (car lst)
              (list-head (cdr lst) (- n 1))))))

(define (// a b) (quotient  a b))
(define (/@ a b) (remainder a b))
(define (/: a b) (modulo    a b))
(define (/@check a b i)
  (if (zero? i)
      (/@ a b)
      (/@ (* 10 (/@check a b (-- i))) b)))
(define (//check a b i)
  (if (zero? i)
      (// a b)
      (// (* 10 (/@check a b (-- i))) b)))
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

(define call/out call-with-output-file)
(define call/in  call-with-input-file)
(define (status name)
  (string-append "./data/" (symbol->string name)))
(define (save->file name proc)
  (call/out (status name) (lambda (x) (write proc x))) 'ok)
(define (save->data name proc . opt)
  (define (nl-add lst x)
    "newline add"
    (let* ((fn0 (lambda (k) (list-ref lst k)))
           (fn1 (lambda (y) (write y x)
                        (write-char #\space x)))
           (store (lambda (z)
                    (if (and (null? opt) (list? (fn0 z)))
                        (map fn1 (fn0 z))
                        (write   (fn0 z) x)))))
      (let loop ((i 0))
        (cond ((= i (length lst)) 'ok)
              (else (store i) (newline x) (loop (++ i)))))))
  (call/out (status name) (lambda (x) (nl-add proc x))))
(define target?
  (lambda (proc x)
    (lambda (y) (proc y x))))

(define fact
  (lambda (n)
    (define fact-iter
      (lambda (y p)
        (if (zero? y)
            p
            (fact-iter (- y 1) (* y p)))))
    (fact-iter n 1)))
(define comb
  (lambda (n r)
    (/ (fact n)
       (* (fact r)
          (fact (- n r))))))

(define (output lst)
  (let loop ((i 0))
    (newline)
    (if (= i (length lst))
        'end
        (begin (display (list-ref lst i))
               (loop (++ i))))))

(define trans48
  (lambda (x) (+ x 48)))
(define digit->string
  (lambda (lst)
    (list->string
     (map integer->char (map trans48 lst)))))
(define (decimal lst)
  (string-append
   (number->string (caadr lst)) "."
   (digit->string (cdadr lst))))

(define (memoize proc)
  (let ((table '()))
    (lambda x
      (let ((stock (assoc x table)))
        (if stock
            (cdr stock)
            (let ((data (apply proc x)))
              (set! table
                    (cons (cons x data) table)) data))))))

(define (/@zero? x y)
  (zero? (/@ x y)))
(define (minid-of n)
  (define (next k)
    (if (= k 2) 3 (+ k 2)))
  "minimum divisor"
  (let ((i 2))
    (let loop ((i i))
      (cond ((< n (sq i))  n)
            ((/@zero? n i) i)
            (else (loop (next i)))))))
(define (wall lst)
  (define pivot?
    (lambda (x) (lambda (y) (= x y))))
  (let loop ((lst1 '()) (lst2 lst))
    (if (null? lst2)
        (reverse lst1)
        (loop
         (cons (filter (pivot? (car lst2)) lst2) lst1)
         (remove (pivot? (car lst2)) lst2)))))
(define (factp-of n)
  "factors of prime number"
  (let loop ((i n) (tmp '()))
    (let ((div (minid-of i)))
      (if (= i div)
          (reverse (cons i tmp))
          (loop (/ i div) (cons div tmp))))))
(define (fact-exp lst)
  (let loop ((lst1 '()) (lst2 lst))
    (if (null? lst2)
        (reverse lst1)
        (loop
         (cons (list (caar lst2) (length (car lst2)))
               lst1)
         (cdr lst2)))))
(define (factorize-of n)
  ((compose fact-exp wall factp-of) n))

(define (rowvec? x)
  "単純なリスト形式ならば行ベクトル"
  (and (list? x) (number? (car x))))
(define (colvec? x)
  "先頭に一要素だけを含むリストを持つ二重リストは列ベクトル"
  (and (list? x) (list? (car x))
       (= 1 (length (car x)))))
(define (mat? x)
  "先頭に二要素以上を含む二重リストは行列"
  (and (list? x) (list? (car x))
       (< 1 (length (car x)))))
(define (mat-t mat)
  (define (mat-form mat)
    (map (lambda (y) (map (lambda (x) (list-ref x y)) mat))
         (iota 0 (- (length (car mat)) 1))))
  (define (vec-form mat)
    (if (list? (car mat))
        (flatten mat)
        (map (lambda (x) (list (list-ref mat x)))
             (iota 0 (- (length mat) 1)))))
  (cond ((mat? mat) (mat-form mat))
        (else (vec-form mat))))
(define (dot* v1 v2)
  (cond ((and (number? v1) (number? v2)) (* v1 v2))
        ((or  (number? v1) (number? v2)) (sca*vec v1 v2))
        ((and (colvec? v1) (rowvec? v2)) (col*row v1 v2))
        ((and (rowvec? v1) (colvec? v2)) (row*col v1 v2))
        (else (row*col v1  (mat-t   v2)))))
(define (sca*vec sca vec)
  (let* ((dummy sca)
         (judge? (and (number? sca) (list? vec)))
         (sca (if judge? sca vec))
         (vec (if judge? vec dummy))
         (calc1 (lambda (x y)
                  (map (lambda (t) (* x t)) y)))
         (calc2 (lambda (x y)
                  (map (lambda (t) (list (* x (car t)))) y))))
    (if (number? (car vec))
        (calc1 sca vec)
        (calc2 sca vec))))
(define (col*row v1 v2)
  (define (row-calc lst)
    (map (lambda (x) (* (car lst) x)) (cadr lst)))
  (map row-calc
       (map (lambda (x) (list x v2)) (mat-t v1))))
(define (row*col v1 v2)
  (apply + (map * v1 (flatten v2))))

(define (mat* m1 m2)
  (cond ((and (mat?    m1) (mat?    m2)) (mat*mat m1 m2))
        ((and (rowvec? m1) (mat?    m2)) (vec*mat m1 m2))
        ((and (mat?    m1) (colvec? m2)) (mat*vec m1 m2))
        ((or (number?  m1) (number? m2)) (sca*mat m1 m2))
        (else (dot* m1 m2))))
(define (mat*mat m1 m2)
  (let ((columns (mat-t m2)))
    (map (lambda (x)
           (map (lambda (y) (dot* x y)) columns)) m1)))
(define (vec*mat vec mat)
  (map (lambda (x) (dot* vec x)) (mat-t mat)))
;;; TODO 元の定義を確認して巻き直す
;;;(define (mat*vec mat vec)
;;;  (mat-t (map (lambda (x) (dot* vec x)) mat)))
(define (mat*vec mat vec)
  (mat-t (vec*mat (mat-t vec) (mat-t mat))))
(define (sca*mat sca mat)
  (let* ((dummy sca)
         (judge? (and (number? sca) (list? mat)))
         (sca (if judge? sca mat))
         (mat (if judge? mat dummy)))
    (map (lambda (x) (dot* sca x)) mat)))

(define (einheit n)
  (let ((lst (iota n)))
    (map (lambda (i)
           (map (lambda (x) (if (= x i) 1 0)) lst))
         lst)))
(define (mat** sm n)
  (let ((mat-sq (lambda (x) (mat*mat x x))))
    (cond ((= n 0) (einheit (length sm)))
          ((= n 1) sm)
          ((even? n) (mat-sq     (mat** sm (/ n 2))))
          ((odd?  n) (mat*mat sm (mat** sm (- n 1)))))))
(define (mat+ m1 m2)
  (cond ((and (mat? m1) (mat? m2)) (map vec+ m1 m2))
        (else (vec+ m1 m2))))
(define (mat- m1 m2) (mat+ m1 (mat* -1 m2)))
(define (vec+ v1 v2)
  (let ((calc (lambda (x y)
                (map (lambda (t) (apply + t))
                     (map list x y)))))
    (cond ((and (rowvec? v1) (rowvec? v2)) (calc v1 v2))
          ((and (colvec? v1) (colvec? v2))
           (mat-t (calc (mat-t v1) (mat-t v2)))))))

(define (fib n)
  (define (fib-iter s1 s2 i)
    (if (zero? i)
        s2
        (fib-iter (+ s1 s2) s1 (-- i))))
  (fib-iter 1 0 n))

(define (colors x)
  (let ((rgb (lambda (r g b)
               (+ (* 65536 r) (* 256 g) b))))
    (cond ((= x  0) (rgb 255 255   0)) ; yellow
          ((= x  1) (rgb   0   0   0)) ; black
          ((= x  2) (rgb 255 165   0)) ; orange
          ((= x  3) (rgb 154 205  50)) ; yellow green
          ((= x  4) (rgb   0 255   0)) ; green
          ((= x  5) (rgb 173 216 230)) ; light blue
          ((= x  6) (rgb 160  32 240)) ; purple
          ((= x  7) (rgb 255   0   0)) ; red
          ((= x  8) (rgb 165  42  42)) ; brown
          ((= x  9) (rgb   0   0 255)) ; blue
          ((= x 10) (rgb 255 192 203)) ; pink
          ((= x 11) (rgb 255 255 255)) ; white
          (else 'fail))))

(define (pascalcolor n m)
  (map (lambda (x)
         (list (car x)
               (cadr x)
               (colors (caddr x))))
       (plot-data n m)))

(define (gene16 seed type)
  (let ((x seed))
    (lambda ()
      (let ((a 1664525)
            (c 1013904223)
            (m (** 2 32))
            (k (** 2 16)))
        (set! x (/: (+ (* a x) c) m))
        (* (** (/ 1 k) type) (// x k))))))
(define random-int  (gene16 1 0))
(define random-frac (gene16 1 1))
(define random-real (gene16 1 1.0))
(define (random n) (/: (random-int) n))
(define (random+ n) (+ 1 (/: (random-int) n)))

(define (rand2d k)
  (map (lambda (x) (list (random-real) (random-real)))
       (iota k)))

(define (sqn? n)
  "square number"
  (if (odd? (tau-of n))
      #t
      #f))

(define sq  (lambda (x) (* x x)))
(define sq+ (lambda (i j) (+ (sq i) (sq j))))
(define (triple n)
  (apply append
         (map (lambda (i)
                (map (lambda (j)
                       (list (sq+ i j) i j))
                     (iota (- i 1))))
              (iota n))))

(define (tau-of n)
  (define (fact-list lst)
    (if (null? lst)
        '()
        (cons (+ (cadar lst) 1)
              (fact-list (cdr lst)))))
  (if (= n 1)
      1
      (apply * ((compose fact-list factorize-of) n))))

(define reverse-all
  (lambda (lst)
    (if (nonpair? lst)
        lst
        (append (reverse-all (cdr lst))
                (list (reverse-all (car lst)))))))

(define (prime? n)
  (define (search n)
    (let loop ((i 2))
      (cond ((< n (* i i)) n)
            ((zero? (/@ n i)) i)
            (else (loop (++ i))))))
  (if (= n 1)
      #f
      (= n (search n))))

(define flat-in
  (lambda (lst)
    (if (null? lst)
        '()
        (append (car lst)
                (flat-in (cdr lst))))))

(define fail '())
(define choose
  (lambda x
    (if (null? x)
        (fail)
        (call/cc (lambda (c-cc)
                   (push (lambda () (c-cc (apply choose (cdr x)))))
                   (car x))))))

(define (Pythagorean? x y z)
  "全体が定数倍になっている自明な組は除くようにしてある: gcd=1? の部分"
  (and (< x y)
       (gcd=1? x y z)
       (= (+ (** x 2) (** y 2)) (** z 2))))
(define (gcd=1? x y z)
  (if (= (gcd x y z) 1) #t #f))

(define (sieve n)
  (map (lambda (x) (if (prime? x) x 0))
       (iota 2 n)))
(define (primes n)
  (filter positive? (sieve n)))

(define sq**
  (lambda (b n)
    (define sq (lambda (x) (* x x)))
    (cond ((zero? n) 1)
          ((even? n) (sq  (sq** b (/ n 2))))
          ((odd?  n) (* b (sq** b (- n 1)))))))

(define nonzero?
  (lambda (x) (not (zero? x))))

(define-syntax nand
  (syntax-rules ()
    ((_) #f)
    ((_ p q ...) (not (and p q ...)))))

(define (sigma-of n)
  (apply + (factors-of n)))

(define (factors-of n)
  "処理速度は遅いので注意"
  (let loop ((i n) (lst '()))
    (cond ((zero? i) lst)
          ((/@zero? n i) (loop (-- i) (cons i lst)))
          (else          (loop (-- i)         lst)))))

(define (fermat? a p)
  (= (/: (** a p) p) a))

(define flatmap
  (lambda (proc lst)
    (apply append (map proc lst))))

(define (/:** a p m)
  (cond ((zero? p) 1)
        ((even? p) (/: (sq  (/:** a (/ p 2) m)) m))
        ((odd?  p) (/: (* a (/:** a (- p 1) m)) m))))

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
(define (p-factor num)
  (map (lambda lst (caar lst))
       (factorize-of num)))

(define shift-encode
  (lambda (str k)
    (define (e-engine lst k)
      (if (null? lst)
          '()
          (cons (+ k (char->integer (car lst)))
                (e-engine (cdr lst) k))))
    (e-engine (string->list str) k)))
(define shift-decode
  (lambda (num k)
    (define (d-engine lst k)
      (if (null? lst)
          '()
          (cons (integer->char (+ (car lst) k))
                (d-engine (cdr lst) k))))
    (list->string (d-engine num k))))

(define map-encode
  (lambda (str k)
    (map (lambda (x) (+ x k))
         (map char->integer (string->list str)))))
(define map-decode
  (lambda (num k)
    (list->string (map integer->char
                       (map (lambda (x) (+ x k)) num)))))
