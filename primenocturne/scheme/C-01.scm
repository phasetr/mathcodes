"P.469"
"付録 C プログラミング言語の拡張"
(define non (lambda (x) 'non))
(map (compose print non) '(1 2 3))
(define id (lambda (x) x))
(map (compose print id) '(1 2 3))
(define add1 (lambda (n) (+ n 1)))
(map (compose print add1) '(1 2 3))
(define sub1 (lambda (n) (- n 1)))
(map (compose print sub1) '(1 2 3))

"P.470"
"C.1 関数を作る"
(define ++ (lambda (i) (+ i 1)))
(map (compose print ++) '(1 2 3))
(define -- (lambda (i) (- i 1)))
(map (compose print --) '(1 2 3))

"組み込み関数の再定義"
(define ** (lambda (a b) (expt a b)))
(define ^ (lambda (a b) (expt a b)))
(** 2 3)
(^ 2 3)

(define // (lambda (a b) (quotient a b)))
(// 9 3)
(// 8 2)
(define /@ (lambda (a b) (remainder a b)))
(/@ 9 4)
(define /: (lambda (a b) (modulo a b)))
(/: 9 3)
(/: 9 5)

"P.471"
"偶奇性"
(define make-even (lambda (n) (* 2 n)))
(define make-odd  (lambda (n) (+ (* 2 n) 1)))
(map (compose print make-even) '(0 1 2 3))
(map (compose print make-odd) '(0 1 2 3))

(define parity-of
  (lambda (p)
    (if (odd? p) -1 1)))
(map (compose print parity-of) '(0 1 2 3 4 5))

"swap"
(define swap-test
  (lambda (a b)
    (** a b)))
"P.472"
(swap-test 2 3)

(define swap-test
  (lambda (a b)
    (let* ((dummy a) (a b) (b dummy))
      (** a b))))
(swap-test 2 3)

"誤差の修正"
"rpund: 偶数丸め・偶捨奇入"
(round 1.5)
(round 3/2)
(round 2.5)
(round 5/2)

"P.473"
(define adjust-of
  (lambda (x)
    (let ((digit 1000)
          (slide (if (positive? x) 1/2 -1/2)))
      (/ (truncate (+ slide (* digit x))) digit))))
(map (compose print adjust-of)
     '(0.2994 -0.2994))

(define adjust-of2
  (lambda (x digit)
    (let ((slide (if (positive? x) 1/2 -1/2)))
      (/ (truncate (+ slide (* digit x))) digit))))
(adjust-of2 0.2994 100)
(adjust-of2 0.2994 1000)
(adjust-of2 -0.2994 100)
(adjust-of2 -0.2994 1000)

"C.1.1 数のリスト"
"P.474"
(define iota
  (lambda (min max)
    (if (> min max)
        '()
        (cons min (iota (+ min 1) max)))))
(equal? (iota 0 3) '(0 1 2 3))
(equal? (iota 1 4) '(1 2 3 4))

"P.475"
(iota 1 9)
(iota -5 5)

(define iota-reverse
  (lambda (min max)
    (if (> min max)
        '()
        (cons max (iota-reverse min (- max 1))))))
(iota-reverse -5 5)

"named let"
(define iota
  (lambda (min max)
    (let iota-loop ((i max) (tmp '()))
      (if (< i min)
          tmp
          (iota-loop (- i 1) (cons i tmp))))))
(iota 2 9)

(define iota-reverse
  (lambda (min max)
    (let iota-loop ((i min) (tmp '()))
      (if (> i max)
          tmp
          (iota-loop (+ i 1) (cons i tmp))))))
(iota-reverse 0 3)
(iota-reverse -5 5)

"P.477"
"生成されたリストによる判断"
(equal? (and #t #t) #t)
(equal? (and #t #f) #f)
(equal? (and #f #t) #f)
(equal? (and #f #f) #f)

(define iota-reverse
  (lambda (min max)
    (let iota-loop ((i min) (tmp '()))
      (if (and (not (null? tmp)) (= (car tmp) max))
          tmp
          (iota-loop (+ i 1) (cons i tmp))))))
(iota-reverse 0 3)
(iota-reverse -5 5)

"P.478"
"C.1.2 選択肢のあるiota"
(define iota
  (lambda (max . opt)
    (let* ((min (if (null? opt) 1 (car opt)))
           (step (if (or (null? opt) (null? (cdr opt)))
                     1 (cadr opt)))
           (dummy max)
           (max (if (null? opt) max min))
           (min (if (null? opt) min dummy)))
      (let loop ((i (- min step)) (tmp '()))
        (if (< (- max step) i)
            (reverse tmp)
            (loop (+ i step)
                  (cons (adjust-of (+ i step)) tmp)))))))
"P.480"
(equal? (iota 9) '(1 2 3 4 5 6 7 8 9))
(equal? (iota 4 9) '(4 5 6 7 8 9))
(equal? (iota 2 9 3) '(2 5 8))

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
(equal? (iota 9) '(1 2 3 4 5 6 7 8 9))
(equal? (iota 4 9) '(4 5 6 7 8 9))
(equal? (iota 2 9 3) '(2 5 8))

"P.481"
"C.1.3 再帰による可算・乗算"
(define succ (lambda (list) (cons 1 list)))
(define pred (lambda (list) (cdr    list)))
"P.482"
(equal? (succ '(1 1 1)) '(1 1 1 1))
(equal? (pred '(1 1 1 1)) '(1 1 1))
"加算 plus"
(define plus
  (lambda (x y)
    (if (null? y)
        x
        (succ (plus x (pred y))))))
"P.483"
(equal? (plus '(1 1) '(1 1 1)) '(1 1 1 1 1))

(define plus-num
  (lambda (x y)
    (if (zero? y)
        x
        (add1 (plus-num x (sub1 y))))))
(equal? (plus-num 2 3) 5)

"乗算 mult"
(define mult
  (lambda (x y)
    (if (null? y)
        '()
        (plus x (mult x (pred y))))))
"P.484"
(equal? (mult '(1 1) '(1 1 3)) '(1 1 1 1 1 1))
(equal? (mult '(1) '(1 1)) '(1 1))

"累乗 pows"
(define pows
  (lambda (x y)
    (if (null? y)
        '(1)
        (mult x (pows x (pred y))))))
(equal? (pows '(1 1) '(1 1 1)) '(1 1 1 1 1 1 1 1))

"無限集合との関係"
(define kakko
  (lambda (i)
    (if (zero? i)
        (list)
        (append (list (kakko (-- i)))
                (kakko (-- i))))))
"P.485"
(equal? (kakko 0) '())
(equal? (kakko 1) '(()))
(equal? (kakko 2) '((()) ()))
(equal? (kakko 3) '(((()) ()) (()) ()))
(add1 0)
(add1 (add1 0))
(add1 (add1 (add1 0)))
((compose add1 add1 add1) 0)
