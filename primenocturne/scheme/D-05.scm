(load "./lib")
"P.648"
"D.5 コラッツの問題"
(define (collatz n)
  (cond ((= n 1) '(1))
        ((even? n) (cons n (collatz (/ n 2))))
        (else      (cons n (collatz (+ (* 3 n) 1))))))
(collatz 7)

"停止問題"
(collatz 27)
"P.649"
(length (collatz 27))
(apply max (collatz 27))
(save->data 'P648-collatz27.dat (collatz 27))

(length (filter  odd? (collatz 27)))
(length (filter even? (collatz 27)))

"sort"
(define (filter-max lst)
  (filter (target? = (apply max lst)) lst))
(define (remove-max lst)
  (remove (target? = (apply max lst)) lst))

(define (sort lst)
  (let loop ((sub lst) (add '()))
    (if (null? sub)
        add
        (loop (remove-max sub)
             (append (filter-max sub) add)))))
(sort (collatz 7))

"初期値の集団"
(define (peak n)
  (map (lambda (i) (apply max (collatz i)))
       (iota 1 n)))
"P.651"
(peak 27)

(save->data 'P651-peak99.dat (peak 99))
;;; just? の定義がない: https://gist.github.com/phasetr/c11ea0fd4085f73cf1c62a2d2e2bca34
(define (just? x) (lambda(y) (= x y)))
(length (filter (just? 9232) (peak 999)))

"成長率を調べる"
"P.652"
(define (steps n)
  (map (lambda (i) (length (collatz i)))
       (iota 1 n)))
(save->data 'P652-steps99.dat (steps 99))
(save->data 'P652-steps999.dat (steps 999))
(save->data 'P652-sort999.dat (sort (peak 999)))
