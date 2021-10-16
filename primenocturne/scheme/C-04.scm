(load "./lib")
"P.506"
"C.4 高階手続"
(define iota
  (lambda lst
    (let* ((x (length lst))
           (max  (if (= 1 x) (car   lst) (cadr lst)))
           (min  (if (< 1 x) (car   lst) 1))
           (step (if (= 3 x) (caddr lst) 1)))
      (define adjust-of
        (lambda (x)
          (let ((digit 1000)
                (slide (if (positive? x) 1/2 -1/2)))
            (/ (truncate (+ slide (* digit x))) digit))))
      (let loop ((i (- min step)) (tmp '()))
        (if (< (- max step) i)
            (reverse tmp)
            (loop (+ i step)
                  (cons (adjust-of (+ i step)) tmp)))))))
(iota 9)

(define num0-9 (iota 0 9))
(define num1-9 (iota 9))
num1-9
(cons 0 num1-9)
(cdr num0-9)
num0-9
num1-9

"C.4.1 apply"
(+ 1 2 3 4 5 6 7 8 9)

"P.507"
(equal? 45 (apply + num1-9))
(apply * num1-9)
(apply - num1-9)
(apply / num1-9)
(apply < num1-9)
(apply = num1-9)
(apply * '(1 2 3))
(apply * 4 5 '(1 2 3))

"P.508"
(apply < 5 num1-9)
(apply - 120 num1-9)
(apply / 120 num1-9)

"C.4.2 mapによる手続の分配"
(map - num1-9)
(map / num1-9)

(define make-even (lambda (n) (* 2 n)))
(define make-odd  (lambda (n) (+ (* 2 n) 1)))
(map make-even num0-9)
(map make-odd num0-9)

(define add1
  (lambda (x) (+ 1 x)))
(map add1 (map make-even num0-9))

"P.509"
"要素同士の計算"
(equal? '((1 3 5) (2 4 6)) (map list '(1 2) '(3 4) '(5 6)))
(equal? '(14 33 65) (map * '(2 3 5) '(7 11 13)))

(map * num1-9 num1-9)
(map * num1-9 num1-9 num1-9)
(map / num1-9 num1-9)

(apply + (map / num1-9 num1-9))

"P.510"
(define check-of
  (lambda (k)
    (if (eq? k k) 1 0)))
(define words
  (lambda (n)
    (apply + (map check-of n))))
(define fruits
  (list 'apple 'orange 'kiwi 'grape 'tomato))
(words fruits)

(let ((x 2) (y 3))
  (map display (list "(+ " x " " y ") " "is equal to "))
       (+ x y))

"P.511"
"map の再定義"
(define map-unit
  (lambda (proc lst)
    (if (null? lst)
        '()
        (cons (proc (car lst))
              (map-unit proc (cdr lst))))))
(define map-mult
  (lambda (proc rest)
    (if (null? (car rest))
        '()
        (cons (apply     proc (map-unit car rest))
              (apply map proc (map-unit cdr rest))))))

(define map
  (lambda (proc . rest)
    (if (null? (cdr rest))
        (map-unit proc (car rest))
        (map-mult proc      rest))))

"P.512"
(map even? '(1 2 3 4 5 6 7 8 9))
(map * '(2 3 5) '(7 11 13) '(17 19 23))
;(map * '(2 3 5) '(7 11 13) '(17 19)) ; エラーになる

"for-each"
(for-each
 (lambda (x) (display x) (display " "))
 '(1 3 5 7))
(define (for-each proc lst)
  (if (null? lst)
      '<unspecified>
      (begin (proc (car lst))
             (for-each proc (cdr lst)))))

"P.513"
"要素の取捨選択"
(equal? (list-tail num1-9 5) '(6 7 8 9))
(equal? (list-tail fruits 3) '(grape tomato))
(equal? (list-ref num1-9 5) 6)
(list-ref fruits 3)

(let ((lst '(1 2 3 4)))
  (cons (car lst) (cdr lst)))

(define filter
  (lambda (predi lst)
    (cond ((null? lst) '())
          ((predi (car lst))
           (cons (car lst)
                 (filter predi (cdr lst))))
          (else (filter predi (cdr lst))))))
(equal? (filter even? num0-9) '(0 2 4 6 8))
(equal? (filter odd?  num0-9) '(1 3 5 7 9))

"P.514"
(equal? (filter odd? (filter even? num0-9)) '())

(define remove
  (lambda (predi lst)
    (cond ((null? lst) '())
          ((predi (car lst))
           (remove predi (cdr lst)))
          (else (cons (car lst)
                      (remove predi (cdr lst)))))))
(equal? (remove even? num0-9) '(1 3 5 7 9))
(equal? (remove odd?  num0-9) '(0 2 4 6 8))

(define target?
  (lambda (proc x)
    (lambda (y) (proc y x))))
(filter (target? = 5) num0-9)
(remove (target? = 5) num0-9)
(filter (target? < 5) num0-9)
(remove (target? < 5) num0-9)

"mapによる手続の入れ子"
"P.515"
((lambda (i)
   ((lambda (j) (list i j)) '1))
 'a)
(map (lambda (i)
       (map (lambda (j) (list i j)) '(1 2)))
     '(a b))

(apply append
       (map (lambda (i)
              (map (lambda (j)
                     (list i j))
                   '(1 2)))
            '(a b)))

(define flatmap
  (lambda (proc lst)
    (apply append (map proc lst))))
(flatmap (lambda (i)
           (map (lambda (j)
                  (list i j)) '(1 2))) '(a b))

"P.516"
(define (double n)
  (apply append
         (map (lambda (i)
                (map (lambda (j) (list i j))
                     (iota (- i 1))))
              (iota n))))
(double 4)
(define sq  (lambda (x) (* x x)))
(define sq+ (lambda (i j) (+ (sq i) (sq j))))
(define (triple n)
  (apply append
         (map (lambda (i)
                (map (lambda (j)
                       (list (sq+ i j) i j))
                     (iota (- i 1))))
              (iota n))))
(triple 4)

"P.517"
"distribution map"
(define dismap
  (lambda (proc dlst)
    (map (lambda (x) (map proc x)) dlst)))
(dismap sq '((2 3) (5 7)))

(dismap (lambda (x) (adjust-of (sqrt x))) '((2 3) (5 7)))

"C.4.4 要素の並べ方"
(define del-obj
  (lambda (lst obj)
    (call/cc
     (lambda (k)
       (cond ((null? lst) '())
             ((equal? (car lst) obj) (k (cdr lst)))
             (else (cons (car lst)
                         (del-obj (cdr lst) obj))))))))
(equal? (del-obj '(1 2 3 4) 1) '(2 3 4))

(define (permutations lst)
  (if (null? lst)
      (list '())
      (apply append
             (map (lambda (i)
                    (map (lambda (j) (cons i j))
                         (permutations (del-obj lst i))))
                  lst))))
(permutations '(1 2 3))
"P.519"
(length (permutations '(1)))
(length (permutations '(1 2)))
(length (permutations '(1 2 3)))
(length (permutations '(1 2 3 4)))
(length (permutations '(1 2 3 4 5)))

"メモ化したい"
(define perm
  (lambda (n r)
    (cond ((= r 0) 1)
          ((= r 1) n)
          (else (* n (perm (- n 1) (- r 1)))))))
"メモ化したい"
(define comb
  (lambda (n r)
    (if (or (= r 0) (= r n))
        1
        (+ (comb (- n 1) (- r 1))
           (comb (- n 1) r)))))
(define rept
  (lambda (n r)
    (if (or (= r 0) (= n 1))
        1
        (+ (rept n (- r 1))
           (rept (- n 1) r)))))
(define comb
  (lambda (n r)
    (/ (perm n r)
       (perm r r))))
(define rept
  (lambda (n r)
    (/ (perm (- (+ n r) 1) r)
       (perm r r))))
(define !n (lambda (n) (perm n n)))
(map (lambda (x) (perm 4 x)) '(0 1 2 3 4))
(map (lambda (x) (comb 4 x)) '(0 1 2 3 4))
(map (lambda (x) (rept 4 x)) '(0 1 2 3 4))
(map !n '(0 1 2 3 4))

"P.521"
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
(map (lambda (x) (comb 4 x)) '(0 1 2 3 4))
(define rept
  (lambda (n r)
    (/ (fact (- (+ n r) 1))
       (* (fact r)
          (fact (- n 1))))))
(map (lambda (x) (rept 4 x)) '(0 1 2 3 4))

"C.4.5 リストによる数値計算"
(define parity-of
  (lambda (p)
    (if (odd? p) -1 1)))
(map parity-of num1-9)
(define par1-9 '(-1 1 -1 1 -1 1 -1 1 -1))
(apply + par1-9)

"確定数と限定数"
(map / (map * num1-9 num1-9))
(apply + (map / (map * num1-9 num1-9)))
"P.522"
(exact->inexact (apply + (map / (map * num1-9 num1-9))))
(* 1.0 (apply + (map / (map * num1-9 num1-9))))

"ゼータの値を求める"
(* 1.0 (apply + (map / (map * num1-9 num1-9))))
(* 1.0 (apply + (map / (map * num1-9 num1-9 num1-9))))
(* 1.0 (apply + (map / (map * num1-9 num1-9 num1-9 num1-9))))
(* 1.0 (apply + (map / (map * num1-9 num1-9 num1-9 num1-9 num1-9))))
(* 1.0 (apply + (map / (map * num1-9 num1-9 num1-9 num1-9 num1-9 num1-9))))

(define ** (lambda (a b) (expt a b)))
(define exp-6 (lambda (x) (** x -6)))
(* 1.0 (apply + (map exp-6 num1-9)))

"P.523"
(define pi (* 4 (atan 1)))
(/ (** pi 2) 6)
(* 1.0 (apply + (map / (map * num1-9 num1-9))))
(/ (** pi 4) 90)
(* 1.0 (apply + (map / (map * num1-9 num1-9 num1-9 num1-9))))
(/ (** pi 6) 945)
(* 1.0 (apply + (map / (map * num1-9 num1-9 num1-9 num1-9 num1-9 num1-9))))

"C.4.6 総和と積"
(begin
  (define num-x (iota 9))
  (define parity-of (lambda (p) (if (odd? p) -1 1)))
  (define parity-x (map parity-of num-x))
  (define terms (map / (map * (map - parity-x) num-x)))
  (* 1.0 (apply + terms)))

"P.525"
"総和記号のコード化"
(define prototype
  (lambda (n k)
    (if (> n k)
        0
        (+ (/ 1.0 (* (** -1 (- n 1)) n))
           (prototype (++ n) k)))))
"P.526"
(define sum
  (lambda  (initial final body)
    (if (> initial final)
        0
        (+ (body initial)
           (sum (++ initial) final body)))))
(define log2
  (lambda (n) (/ (** -1 (- n 1)) n)))
(* 1.0 (sum 1 1000 log2))

"P.527"
"無限級数と無限乗積"
(define leibniz
  (lambda (n) (/ (** -1 n) (+ (* 2 n) 1))))
(* 4.0 (sum 0 1000 leibniz))

(define zeta2
  (lambda (n) (/ (** n 2))))
(* 1.0 (sum 1 1000 zeta2))

(define product
  (lambda (initial final body)
    (if (> initial final)
        1
        (* (body initial)
           (product (++ initial) final body)))))
"P.528"
(define pi/4
  (lambda (n)
    (* (/ (* 2 n) (+ (* 2 n) 1))
       (/ (+ (* 2 n) 2) (+ (* 2 n) 1)))))
(* 4.0 (product 1 1000 pi/4))

(define fact
  (lambda (i) (if (= i 0) 1 i)))
(product 0 10 fact)

"accumulate"
(define accumulate
  (lambda (op ini seqs)
    (if (null? seqs)
        ini
        (op (car seqs)
            (accumulate op ini (cdr seqs))))))
"P.529"
(define sum
  (lambda (ini fin body)
    (accumulate + 0 (map body (iota ini fin)))))
(define product
  (lambda (ini fin body)
    (accumulate * 1 (map body (iota ini fin)))))
(let ((num (lambda (i) i)))
  (sum 0 100 num))

(define sum10
  (lambda (ini fin body)
    (accumulate + 0 (map body (iota ini fin 10)))))
(let ((num (lambda (i) i)))
  (sum10 0 100 num))

"ネイピア数"
(define napier
  (lambda (n)
    (/ (product 0 n fact))))
(* 1.0 (sum 0 100 napier))
(exp 1)

"冪乗数を抽出する"
(map (lambda (x) (* x x)) (iota 10))
(map (lambda (x) (* x x x)) (iota 10))

(define pow2?
  (lambda (x)
    (let ((y (inexact->exact (round (** x 1/2)))))
      (if (= x (** y 2)) #t #f))))
"P.531"
(filter pow2? (iota 100))
(define pow2-list
  (lambda (n)
    (let loop ((i n) (tmp '()))
      (cond ((= i 0) tmp)
            ((pow2? i) (loop (- i 1) (cons i tmp)))
            (else      (loop (- i 1)         tmp))))))
(pow2-list 100)

(define pow3?
  (lambda (x)
    (let ((y (inexact->exact (round (** x 1/3)))))
      (if (= x (** y 3)) #t #f))))
(filter pow3? (iota 1000))
(member 343 (map (lambda (x) (* x x x)) (iota 10)))

"C.4.7 文字と数字"
"P.532"
(char? #\G)
(char? "G")
(char? "Gauss")

(string? #\G)
(string? "G")
(string? "Gauss")

(string-ref "Gauss" 0)

"数を数字に"
(char->integer #\0)
(define trans48
  (lambda (x) (+ x 48)))
(define ten (iota 0 9))
(map trans48 ten)
(map integer->char (map trans48 ten))

"P.533"
(define digit->string
  (lambda (lst)
    (list->string
     (map integer->char (map trans48 lst)))))
(digit->string ten)

"文字を数値に"
(char->integer #\A)
(char->integer #\Z)
(char->integer #\space)
(char->integer #\.)
(char->integer #\,)
(char->integer #\_)

"P.534"
(define ul-exchange
  (lambda (character)
    (let ((x (char->integer character)))
      (cond ((and (<= 65 x) (>= 90 x))
             (integer->char (+ x 32)))
            ((and (<= 97 x) (>= 122 x))
             (integer->char (- x 32)))
            (else &again!)))))
(ul-exchange #\a)

"シーザー暗号"
(define shift-encode
  (lambda (str k)
    (define (e-engine lst k)
      (if (null? lst)
          '()
          (cons (+ k (char->integer (car lst)))
                (e-engine (cdr lst) k))))
    (e-engine (string->list str) k)))
"P.535"
(shift-encode "Cross the Rubicon!" 3)

(define shift-decode
  (lambda (num k)
    (define (d-engine lst k)
      (if (null? lst)
          '()
          (cons (integer->char (+ (car lst) k))
                (d-engine (cdr lst) k))))
    (list->string (d-engine num k))))
(shift-decode
 (shift-encode "Cross the Rubicon!" 3)
 -3)

(define map-encode
  (lambda (str k)
    (map (lambda (x) (+ x k))
         (map char->integer (string->list str)))))
(define map-decode
  (lambda (num k)
    (list->string (map integer->char
                       (map (lambda (x) (+ x k)) num)))))

(map-decode
 (map-encode "Cross the Rubicon!" 3)
 -3)
