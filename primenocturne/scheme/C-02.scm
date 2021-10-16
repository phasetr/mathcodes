"P.486"
"C.2 ループ不変表明"
(define plus (lambda (x y)
               (if (zero? y) x
                   (+ 1 (plus x (- y 1))))))
(define mult (lambda (x y)
               (if (zero? y) 0
                   (+ x (mult x (- y 1))))))
(define pows (lambda (x y)
               (if (zero? y) 1
                   (* x (pows x (- y 1))))))
(plus 1 2)
(mult 1 2)
(mult 2 3)
(pows 2 3)

"C.2.1 末尾再帰"
"P.487"
"plus の場合"
(define plus-iter
  (lambda (x y p)
    (if (zero? y)
        (+ x p)
        (plus-iter x (- y 1) (+ p 1)))))
(plus-iter 2 3 0)

"P.488"
(define plus-tailrec
  (lambda (x y)
    (define plus-iter
      (lambda (x y p)
        (if (zero? y)
            (+ x p)
            (plus-iter x (- y 1) (+ p 1)))))
    (plus-iter x y 0)))

"P.489"
"mult の場合"
(define mult-tailrec
  (lambda (x y)
    (define mult-iter
      (lambda (x y p)
        (if (zero? y)
            p
            (mult-iter x (- y 1) (+ x p)))))
    (mult-iter x y 0)))
(mult-tailrec 0 1)
(mult-tailrec 3 4)

"pows の場合"
"P.490"
(define pows-tailrec
  (lambda (x y)
    (define pows-iter
      (lambda (x y p)
        (if (zero? y)
            p
            (pows-iter x (- y 1) (* x p)))))
    (pows-iter x y 1)))
(pows-tailrec 1 0)
(pows-tailrec 2 3)

"C.2.2 逐次平方による冪乗計算"
"P.491"
(define sq**
  (lambda (b n)
    (define sq (lambda (x) (* x x)))
    (cond ((zero? n) 1)
          ((even? n) (sq  (sq** b (/ n 2))))
          ((odd?  n) (* b (sq** b (- n 1)))))))
(sq** 2 2)
(sq** 2 4)
(sq** 3 43)
(sq** 82 43)

"末尾再帰への変換"
"P.493"
(define sq**-tailrec
  (lambda (b n)
    (define sq**-iter
      (lambda (b n p)
        (cond ((zero? n) p)
              ((even? n) (sq**-iter (* b b) (/ n 2) p))
              ((odd?  n) (sq**-iter b (- n 1) (* b p))))))
    (sq**-iter b n 1)))
(sq**-tailrec 2 2)
(sq**-tailrec 2 4)
(sq**-tailrec 3 43)
(sq**-tailrec 82 43)

"P.494"
"C.2.3 開錠の計算"
(define fact
  (lambda (n)
    (if (zero? n)
        1
        (* n (fact (- n 1))))))
(define fact-tailrec
  (lambda (n)
    (define fact-iter
      (lambda (y p)
        (if (zero? y)
            p
            (fact-iter (- y 1) (* y p)))))
    (fact-iter n 1)))
(fact-tailrec 25)

"P.495"
"繰り返しの書法"
(define fact-let
  (lambda (n)
    (let countdown ((y n ) (p 1))
      (if (zero? y)
          p
          (countdown (- y 1) (* y p))))))
(fact-let 6)

(define fact-let+
  (lambda (n)
    (let countup ((y 0) (p 1))
      (if (= y n)
          p
          (countup (+ y 1) (* (+ y 1) p))))))
(fact-let+ 8)

(define fact-do
  (lambda (n)
    (do ((y n (- y 1)) (p 1 (* y p)))
        ((zero? y) p))))
(fact-do 5)
