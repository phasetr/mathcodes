(load "./lib")
"P.799"
"D.19 カードから格子点へ"
(define (lattice2 n)
  (let ((lst (iota n)))
    (flatmap (lambda (i)
               (map (lambda (j)
                      (list i j)) lst)) lst)))
(lattice2 4)

(define (lattice3 n)
  (let ((lst (iota n)))
    (flatmap
     (lambda (i)
       (flatmap
        (lambda (j)
          (map (lambda (k)
                 (list i j k)) lst)) lst)) lst)))
(lattice3 4)

"P.800"
"D.19.1 格子点を求める"
"格子点の生成"
(define (lattice n d init)
  (let ((rst '()))
    (let loop ((dim 0) (tmp '()))
      (if (= dim d)
          (set! rst (cons (reverse tmp) rst))
          (do ((k init (+ k 1)))
              ((< n k))
            (loop (+ dim 1) (cons k tmp)))))
    (reverse rst)))
(equal? (lattice2 4) (lattice 4 2 1))
"P.801"
(lattice 4 2 0)
(lattice 4 2 3)
(lattice 2 5 1)

"選択条件を定める"
"P.802"
(define (unique? lst)
  (define (twin=? x)
    (cond ((null? x) #f)
          ((null? (cdr x)) #t)
          (else (and
                 (not (= (car x) (cadr x)))
                 (twin=? (cdr x))))))
  (twin=? (sort lst)))
(map unique? '((1 1) (1 2) (2 1) (2 2) (1 2 4 3 1)))

"D.19.2 格子点の全数探査"
(define (element-pch proc n r)
  (let loop ((lst (lattice n r 1)) (tmp '()))
    (cond ((null? lst) (reverse tmp))
          ((proc (car lst))
           (loop (cdr lst) (cons (car lst) tmp)))
          (else (loop (cdr lst) tmp)))))

"P.803"
(define (element-p n r)
  (element-pch unique? n r))
(define (element-c n r)
  (element-pch (lambda (x) (apply < x)) n r))
(define (element-h n r)
  (element-pch (lambda (x) (apply <= x)) n r))
(element-p 3 2)
(element-c 3 2)
(element-h 3 2)
(define (sieve-p n r) (length (element-p n r)))
(define (sieve-c n r) (length (element-c n r)))
(define (sieve-h n r) (length (element-h n r)))
(sieve-p 3 2)
(sieve-c 3 2)
(sieve-h 3 2)
