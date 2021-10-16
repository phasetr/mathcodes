(load "./lib")
(load "./C-05")
"TODO 本をきちんと写経したつもりだが本に書いてある通りの出力をしてくれず, 動かない."
"面倒なのでいったん放置: 気が向いたら修正する."
"P.764"
"D.15 非決定性計算による解法"
;;;(choose (choose) 1)
(define fail '())
"マクロによる定義"
(define choose
  (lambda (x)
    (if (null? x)
        (fail)
        (call/cc (lambda (c-cc)
                   (push (lambda () (c-cc (choose (cdr x)))))
                   (car x))))))
"P.765"
(choose (iota 2 5))
(choose '())
(choose (choose '()) 1)

;;; TODO choose を正しく定義する
(define fail '())
(define-syntax choose
  (syntax-rules ()
    ((_) (fail))
    ((_ 1st) 1st)
    ((_ 1st 2nd ...)
     (call/cc (lambda (c-cc)
                (push (lambda () (c-cc (choose 2nd ...))))
                1st)))))
(choose (choose) 1)
;;;(choose (choose) 1)
;;; https://www.shido.info/lisp/scheme_amb.html
;;;(define fail #f)
;;;(define-syntax amb
;;;  (syntax-rules ()
;;;    ((_) (fail))
;;;    ((_ a) a)
;;;    ((_ a b ...)
;;;     (let ((fail0 fail))
;;;       (call/cc
;;;        (lambda (cc)
;;;          (set! fail
;;;                (lambda ()
;;;                  (set! fail fail0)
;;;                  (cc (amb b ...))))
;;;          (cc a)))))))
;;;;;; (choose (choose) 1)
;;;(amb (amb) 1)

"リストへの対応"
"P.766"
(define fail '())
;;; TODO 動かない
(define (amb . lst)
  (if (null? lst)
      (fail)
      (let loop ((x (car lst)))
        (if (null? x)
            (fail)
            (choose
             (eval (car x) (interaction-environment))
             (loop (cdr x)))))))
(amb '((amb) 1))

(define-syntax ans-of
  (syntax-rules ()
    ((_ eqs)
     (let ((tmp '()))
       (choose
        (let ((dum eqs))
          (set! tmp (cons dum tmp))
          (fail))
        (reverse tmp))))))
(ans-of (list (choose 2 3) (choose 5 7)))
"P.767"
(ans-of (list (amb '(2 3)) (amb '(5 7))))

"制約のある問題"
(define (amb-num min max)
  (amb (iota min max)))
(amb-num 2 5)
(ans-of (amb-num 2 5))

(define (require predi)
  (if (not predi) (fail)))

(define (p-generator n)
  (let ((i (amb-num 2 n)))
    (require (prime? i)) i))
(p-generator 100)
(amb)
"P.768"
;;; 正しく動かない
(ans-of (p-generator 100))

(define (p-pair min max)
  (let ((i (amb-num min max)))
    (require
     (and (prime? i)
          (prime? (+ i 2))))
    (list i (+ i 2))))
;;; 正しく動かない
(ans-of (p-pair 3 100))

(define (sgp-pair min max)
  (let ((i (amb-num min max)))
    (require
     (and (prime? i)
          (prime? (+ (* i 2) 1))))
    (list i '/ (+ (* i 2) 1))))
"P769"
(ans-of (sgp-pair 2 50))

(define (amb-triple n)
  (let* ((i (amb-num 3 n))
         (j (amb-num i n))
         (k (amb-num j n)))
    (require (Pythagorean? i j k))
    (list i j k)))
(ans-of (amb-triple 50))

(define (amb-105 n)
  (let ((i (amb-num 1 n)))
    (require
     (and (= 1 (/@ i 3))
          (= 2 (/@ i 5))
          (= 3 (/@ i 7))))
    i))
(amb-105 100)
(ans-of (amb-105 1000))
