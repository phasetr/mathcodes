"B.5 型の確認と構文の拡張"
"procedure? number? pair? null? symbol?
boolean? string? char? vector?"
(define type-check
  (lambda (x)
    (define form
      (lambda (str)
        (display "This is ") (display str)))
    (cond ((procedure? x) (form "a procedure: ") x)
          ((number?    x) (form "a number: ")    x)
          ((pair?      x) (form "a pair: ")      x)
          ((null?      x) (form "the empty: ")   x)
          ((symbol?    x) (form "a symbol: ")    x)
          ((string?    x) (form "a string: ")    x)
          ((char?      x) (form "a charactor: ") x)
          ((boolean?   x) (form "a boolean: ")   x)
          ((vector?    x) (form "a vector: ")    x)
          (else (display
                 "may be a special form: ") x))))
"P.450"
(type-check car)
(type-check 3.14)
(type-check '(a b))
(type-check '())
(type-check 'a)
(type-check "ab")
(type-check #\a)
(type-check #t)
(type-check #(1 2))
(type-check if)
(type-check :a)
(type-check type-check)
(type-check (number? 3.14))
(type-check and)
(type-check (and))

"P.451"
"B.5.2 数値データの型"
"number? complex? real? rational? integer? positive?
negative? zero? odd? even? exact? inexact?"
(define type-of
  (lambda (x)
    (define form
      (lambda (str) (display str) (display "/ ")))
    (display "This is ")
    (cond ((number? x) (display "a number: ")
           (cond ((and (real? x) (not (negative? x)))
                  (form "real/ nonnegative"))
                 ((and (real? x) (negative? x))
                  (form "real/ negative"))
                 (else (form "complex")))
           (cond ((and (integer? x) (odd? x))
                  (form "integer/ odd"))
                 ((and (integer? x) (even? x))
                  (form "integer/ even"))
                 (else (form "noninteger")))
           (cond ((exact? x) (form "exact"))
                 (else (form "inexact"))) (display x))
          (else (display "a string: ") (display x)))))
(let ((l (list 2+5i 'symbol 2/5 -0.4 5 5.0 0)))
  (map (lambda (x)
         (type-of x)
         (display "\n")) l))
(let ((pi 3.1415))
  (display "\n")
  (type-of pi)
  (display "\n")
  (type-of #e3.1415)
  (display "\n")
  (type-of #i-2/5)
  (display "\n")
  (type-of (exact->inexact 5))
  (display "\n")
  (type-of (inexact->exact 5)))

;;; 逐次 display ではなく文字列のリスト連結で作った.
;;; set! を乱発しているところはもう少し綺麗にならないか.
(define type-of2
  (lambda (x)
    (let ((strs '("This is")))
      (cond ((number? x) (set! strs (append strs (list "a number:")))
             (cond ((and (real? x) (not (negative? x)))
                    (set! strs (append strs '("real nonnegative"))))
                   ((and (real? x) (negative? x))
                    (set! strs (append strs '("real negative"))))
                   (else (set! strs (append strs '("complex")))))
             (cond ((and (integer? x) (odd? x))
                    (set! strs (append strs '("integer odd"))))
                   ((and (integer? x) (even? x))
                    (set! strs (append strs '("integer even"))))
                   (else (set! strs (append strs '("noninteger")))))
             (cond ((exact? x) (set! strs (append strs '("exact"))))
                   (else (append strs '("inexact"))))
             (append strs '(x)))
            (else (append strs '("a string:") (list x)))))))
(let ((l (list 2+5i 'symbol 2/5 -0.4 5 5.0 0)))
  (print "")
  (map (compose print type-of2) l))

"P.453"
"B.5.3 構文の拡張"
(define-syntax new-if
  (syntax-rules (then else)
    ((new-if predi then consq else altna)
     (if     predi      consq      altna))
    ((new-if predi then consq)
     (if     predi      consq      #f))
    ((new-if predi            else altna)
     (if     predi      #f         altna))))

"P.454"
(new-if #t then 1 else 0)
(if #t 1 0)
(new-if #f then 1)
(new-if #t else 0)

"nand の定義"
(define-syntax nand
  (syntax-rules ()
    ((_) #f)
    ((_ p q ...) (not (and p q ...)))))
"P.455"
(let ()
  (print (nand))
  (print (nand #t))
  (print (nand #f #f))
  (print (nand #t #t))
  (print (nand #t #f))
  (print (nand #t #t #t #t #t #t #t #t #t #f)))

(define-syntax nand
  (syntax-rules ()
    ((_) #f)
    ((_ p) (if p #f #t))
    ((_ p q ...)
     (if p (nand q ...) #t))))
(let ()
  (print (nand))
  (print (nand #t))
  (print (nand #f #f))
  (print (nand #t #t))
  (print (nand #t #f))
  (print (nand #t #t #t #t #t #t #t #t #t #f)))

"inc と dec のて定義"
(define-syntax inc
  (syntax-rules ()
    ((_ i)   (begin (set! i (+ i 1)) i))
    ((_ i k) (begin (set! i (+ i k)) i))))
(define-syntax dec
  (syntax-rules ()
    ((_ i)   (begin (set! i (- i 1)) i))
    ((_ i k) (begin (set! i (- i k)) i))))

"P.456"
(let ()
  (print)
  (print ((lambda (x) (inc x))   3))
  (print ((lambda (x) (inc x 2)) 3))
  (print ((lambda (x) (dec x))   3))
  (print ((lambda (x) (dec x 2)) 3)))

"delayed cons の定義"
"遅延リスト"
(define-syntax s-cons
  (syntax-rules ()
    ((_ x y) (cons x (delay y)))))
(s-cons 'a 'b)

(define s-car (lambda (x) (car x)))
(define s-cdr (lambda (x) (force (cdr x))))
(s-car (s-cons 'a 'b))
(s-cdr (s-cons 'a 'b))
