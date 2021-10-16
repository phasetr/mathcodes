(load "./lib")
"P.555"
"C.7 関数型言語の基礎"
"C.7.1 チャーチの数字"
"zero = KI"
"\mathbb{C}_k"
(define zero  (lambda (s) (lambda (z) z)))
(define one   (lambda (s) (lambda (z) (s z))))
(define two   (lambda (s) (lambda (z) (s(s z)))))
(define three (lambda (s) (lambda (z) (s(s(s z))))))

"P.556"
((two add1) 0)
(add1 (add1 0))

((zero  add1) 0)
((one   add1) 0)
((two   add1) 0)
((three add1) 0)

"compose は B コンビネータと同じ"
;;;(define compose
;;;  (lambda (f g)
;;;    (lambda (x) (f (g x)))))
(define repeated
  (lambda (f n)
    (if (zero? n)
        (lambda (x) x)
        (compose f (repeated f (-- n))))))
((repeated add1 7) 3)

"P.557"
"C.7.2 後任関数"
"単位元"
(define Succ
  (lambda (v) (lambda (s) (lambda (z)
                            (s ((v s) z))))))
((two add1) 0)
(((Succ two) add1) 0)

(define Succ%
  (lambda (v s z) (s ((v s) z))))
(Succ% two add1 0)

"P.559"
"定理と定義"

"P.560"
"C.7.3 加算関数"
"P.561"
(define Plus
  (lambda (u v) (lambda (s) (lambda (z)
                              ((u s) ((v s) z))))))
(((Plus two three) add1) 0)

"P.562"
"C.7.4 乗算関数"
(define Mult
  (lambda (u v) (lambda (s) (lambda (z)
                              ((u (v s)) z)))))
(((Mult two three) add1) 0 )

"P.564"
"C.7.5 累乗関数"
"P.565"
"数学的帰納法による証明"
"P.566"
(define Pows (lambda (u v) (u v)))
(((Pows three two) add1) 0)

"一次関数のラムダ項"
"P.567"
(define Linear
  (lambda (a x b) (lambda (s) (lambda (z)
                                ((b s) ((a (x s)) z))))))
(((Linear two three one) add1) 0)

"C.7.6 条件分岐"
(define *true  (lambda (x) (lambda (y) x)))
(define *false (lambda (x) (lambda (y) y)))
(define *if    (lambda (c m n) ((c m) n)))
(define *zero?
  (lambda (m)
    ((m (lambda (n) *false)) *true)))
"P.569"
(define *cons (lambda (x y) (lambda (c) ((c x) y))))
(define *car  (lambda (x) (x *true)))
(define *cdr  (lambda (x) (x *false)))
(*car (*cons 3 5))
(*cdr (*cons 'a 'b))

"C.7.7 前任関数"
"P.571"
(define Pred
  (lambda (m)
    (lambda (s)
      (lambda (z)
        (((m (lambda (p) (lambda (q) (q (p s)))))
          (lambda (v) z))
         (lambda (v) v))))))
(((Pred three) add1) 0)

"P.572"
(define Mins
  (lambda (u v) ((v Pred) u)))
(((Mins three two) add1) 0)

"C.7.8 Yコンビネータ"
"P.573"
(define Sn
  (lambda (n)
    (if (zero? n)
        0
        (+ n (Sn (-- n))))))
(map Sn (iota 0 9))

"Y の動きを追う"
"P.574"
(define fact
  (lambda (n)
    (if (zero? n)
        1
        (* n (fact (-- n))))))
(fact 3)

"P.575"
"Scheme における Y"
(define Sn
  (lambda (f)
    (lambda (n)
      ((*if (*zero? n)
            (lambda () zero)
            (lambda () (Plus n (f (Pred n)))))))))
"P.576"
"この Y は実際は Z コンビネーター"
"本にはタイポがあるので注意: cf. https://tnomura9.exblog.jp/26145667/"
(define Y
  (lambda (g)
    ((lambda (x) (g (lambda (s) ((x x) s))))
     (lambda (x) (g (lambda (s) ((x x) s)))))))
((((Y Sn) three) add1) 0)

(define Fact
  (lambda (f)
    (lambda (n)
      ((*if (*zero? n)
            (lambda () one)
            (lambda () (Mult n (f (Pred n)))))))))
((((Y Fact) two) add1) 0)

"通常の四則による Y"
(define Fact
  (lambda (f)
    (lambda (n)
      (if (zero? n)
          1
          (* n (f (-- n)))))))
((Y Fact) 5)

((Y (lambda (f)
      (lambda (n)
        (if (zero? n)
            1
            (* n (f (-- n))))))) 5)

"P.577"
"C.7.9 Y の導出への再帰"
"再びYを探す"
(define Fact
  (lambda (y)
    (if (zero? y) 1 (* y (Fact (-- y))))))
(Fact 5)

(define Fact
  (lambda (f)
    (lambda (y)
      (if (zero? y) 1 (* y (f (-- y)))))))
((Y Fact) 5)

"P.578"
(define Fact
  (lambda (f)
    (lambda (y)
      (if (zero? y) 1 (* y ((f f) (-- y)))))))
((Fact Fact) 3)
"P.579"
(((lambda (f)
    (lambda (y)
      (if (zero? y) 1 (* y ((f f) (-- y))))))
  (lambda (f)
    (lambda (y)
      (if (zero? y) 1 (* y ((f f) (-- y))))))) 3)

(define w (lambda (x) (x x)))
((w (lambda (f)
      (lambda (y)
        (if (zero? y) 1 (* y ((f f) (-- y))))))) 3)

"さらに二重化"
(lambda (x) (lambda (s) ((x x) s)))
"P.580"
(lambda (f) (lambda (y) (if (zero? y) 1 (* y (f (-- y))))))
(define Y
  (lambda (g)
    ((lambda (x) (g (lambda (s) ((x x) s))))
     (lambda (x) (g (lambda (s) ((x x) s)))))))

(define quine
  (lambda (x) (list x (list (quote quote) x))))
(quine 'quine)
(quote quote)
(list (quote quote) 999)
(list 999 (list (quote quote) 999))
(list 'x)
(quote x)
