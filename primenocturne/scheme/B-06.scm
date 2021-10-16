"P.457"
"B.6 継続"
"B.6.1 継続渡し形式"
(define postfix
  (lambda (a b proc)
    (proc a b)))

"P.458"
"cps = continuation passing style"
(define cps
  (lambda (a b proc1 proc2)
    (proc2 (proc1 a b))))
"nop = no operation"
(define id (lambda (x) x))

(= (+ 3 5)
   (cps 3 5 + id))
(= (* 3 5)
   (cps 3 5 * id))

(cps 3 5 +
     (lambda (x) (cps 7 x * id)))

"分母が奇数である分数の値を求める手続"
(define cps-div
  (lambda (num den failure1 failure2 failure3 success)
    (cond ((= den   0) (failure1))               ; 分母が 0 ならエラー表示
          ((= den   1) (failure2 num))           ; 分母が 1 なら有理数
          ((even? den) (failure3 den))           ; 分母が偶数なら偶数
          (else        (success (/ num den)))))) ; 分母が奇数のときだけ値を表示
(define frac-odd
  (lambda (num den)
    (cps-div num den
             (lambda ()  (display "error: divided by zero"))
             (lambda (x) (display x) (display ": result is rational"))
             (lambda (x) (display x) (display ": denominator is even"))
             (lambda (x) x))))
(frac-odd 1 0)
(frac-odd 3 1)
(frac-odd 4 2)
(frac-odd 1 3)

"ラムダ項による再検討"
(+ 3 5)
((lambda (k) (+ 3 k)) 5)
((lambda (x) (x 5))
 (lambda (k) (+ 3 k)))

"P.460"
((lambda (x) (x 5))
 (lambda (k) (* 3 k)))

(* 7 (+ 3 5))
((lambda (x) (x (+ 3 5)))
 (lambda (k) (* 7 k)))

"B.6.2 継続の生成"
"call/cc = call-with-current-continuation"
"opp = one paraeter procedure"

"P.462"
(* 7 (+ 3 5))
(* 7 (call/cc (lambda (x) (x (+ 3 5)))))
(lambda (k) (* 7 k))

"P.463"
"B.6.3 継続の機能"
(define opp/cc '())
(* 7 (call/cc (lambda (x) (set! opp/cc x) (x (+ 3 5)))))

(= 56 (+ 1 (opp/cc 8)))
(= 57 (+ 1 ((lambda (k) (* 7 k)) 8)))

(* 3 5 (call/cc (lambda (x) (x 7))))
(opp/cc 1)
(* 3   (call/cc (lambda (x) (x 5))) 7)
(opp/cc 1)
(*     (call/cc (lambda (x) (x 3))) 5 7)
(opp/cc 1)
(call/cc (lambda (x) (* 3 5 7)))
(opp/cc 1)

"P.464"
(call/cc (lambda (x) (set! opp/cc x) (* 3 5 7)))
(opp/cc 'test)

"大域脱出"
"P.465"
;;;(opp/cc k)
;;;=
;;;((lambda (x) (* 7 x)) k)

;;;(+ 1 (opp/cc k))
;;;neq
;;;(+ 1 ((lambda (x) (* 7 x)) k))
(* 3 (call/cc (lambda (x) (+ 5 (x 7)))))
(call/cc (lambda (x) (x (* 7 (+ 3 5)))))
(call/cc (lambda (x) (* (x 7) (+ 3 5))))
(call/cc (lambda (x) (* 7 (x (+ 3 5)))))
(call/cc (lambda (x) (* 7 (+ (x 3 5)))))
(call/cc (lambda (x) (* 7 (+ 3 (x 5)))))

((lambda (x) x) (* 7 (+ 3 5)))
((lambda (x) x) 7)
((lambda (x) x) (+ 3 5))
((lambda (x) x) 3)
((lambda (x) x) 5)

"P.466"
(define branch
  (lambda (s)
    (cond ((= s 1) (call/cc (lambda (x) (* (x 7) (+ 3 5)))))
          ((= s 2) (call/cc (lambda (x) (* 7 (x (+ 3 5))))))
          ((= s 3) (call/cc (lambda (x) (* 7 (+ (x 3) 5)))))
          (else 'again))))
(branch 1)
(branch 2)
(branch 3)

(define product
  (lambda (lst)
    (call/cc
     (lambda (k)
       (cond ((null? lst) 1)
             ((= (car lst) 0) (k 0))
             (else (* (car lst) (product (cdr lst)))))))))
(product '(0 1 2))
(product '(1 2))
