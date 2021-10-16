(load "./lib")
(load "./C-05.scm")
"P.549"
"C.6 非決定計算"
"C.6.1 宣言的知識への移行"

"P.550"
"C.6.2 機能と定義"
(define fail '())
(call/cc
 (lambda (f-cc)
   (set! fail
         (lambda () (if (null? stack)
                        (f-cc 'empty)
                        (((pop))))))))
"P.551"
(define choose
  (lambda x
    (if (null? x)
        (fail)
        (call/cc (lambda (c-cc)
                   (push (lambda () (c-cc (apply choose (cdr x)))))
                   (car x))))))
(set-stack)
(choose)
(choose 'test)

"P.552"
"C.6.3 具体的な働き"
(choose 1 2)

"P.554"
(define odd-killer
  (lambda ()
    (let ((x (choose 1 2)))
      (if (odd? x)
          (fail)
          x))))
(odd-killer)

(define two-nums
  (lambda ()
    (list (choose 2 3)
          (choose 5 7))))
(two-nums)
(fail)

(define three-nums
  (lambda ()
    (list (choose 2 5 13)
          (choose 3 7 11))))
(three-nums)
(fail)
