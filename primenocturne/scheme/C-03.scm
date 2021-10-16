"P.496"
"C.3 リストを調べる"

"C.3.1 恒等関数"
(define id (lambda (x) x))
(id '(1 2 3))

(let ((l '(1 2 3)))
  (cons (car l) (cdr l)))

"P.497"
(define id-updown
  (lambda (lst)
    (if (null? lst)
        '()
        (cons (car lst)
              (id-updown (cdr lst))))))
(id-updown '(1 2 3))

"C.3.2 要素の抽出"
"append"
"P.498"
;;;(define append
;;;  (lambda (lst lst+)
;;;    (if (null? lst)
;;;        lst+
;;;        (cons (car lst)
;;;              (append (cdr lst) lst+)))))
(append '(1 2 3) '(4 5 6))

"length"
(define length
  (lambda (lst)
    (if (null? lst)
        0
        (+ 1 (length (cdr lst))))))
(equal? (length '(1 2 3)) 3)

"list-tail, list-ref, list-head"
(define list-tail
  (lambda (lst n)
    (if (zero? n)
        lst
        (list-tail (cdr lst) (- n 1)))))
(list-tail '(1 2 3 4) 2)
"P.499"
(define list-ref
  (lambda (lst n)
    (if (zero? n)
        (car lst)
        (list-ref (cdr lst) (- n 1)))))
(equal? 0 (list-ref '(1 2 3 4) 0))

(define list-head
  (lambda (lst n)
    (if (zero? n)
        '()
        (cons (car lst)
              (list-head (cdr lst) (- n 1))))))
(list-head '(1 2 3 4) 1)

(define last-pair
  (lambda (lst)
    (list (list-ref lst (- (length lst) 1)))))
(last-pair '(1 2 3 4))
(define last-pair
  (lambda (lst)
    (if (null? (cdr lst))
        lst
        (last-pair (cdr lst)))))

"reverse"
"P.500"
(define reverse
  (lambda (lst)
    (if (null? lst)
        '()
        (append (reverse (cdr lst))
                (list (car lst))))))
(reverse '(1 2 3 4))

"C.3.3 二重再帰"
(define id-updown
  (lambda (lst)
    (if (null? lst)
        '()
        (cons (car lst)
              (id-updown (cdr lst))))))
(id-updown '(1 2 3 4))

"P.501"
(define nonpair? (lambda (x) (not (pair? x))))
(nonpair? '(1))
(nonpair? '())
(null? '())
(define id-all
  (lambda (lst)
    (if (nonpair? lst)
        lst
        (cons (id-all (car lst))
              (id-all (cdr lst))))))
(id-all '(((1 2) 3) 4))

(define length-all
  (lambda (lst)
    (cond ((null? lst) 0)
          ((nonpair? lst) 1)
          (else (+ (length-all (car lst))
                   (length-all (cdr lst)))))))
(length-all '(((1 2) 3) 4))
(length '(((1 2) 3) 4))

(define reverse-all
  (lambda (lst)
    (if (nonpair? lst)
        lst
        (append (reverse-all (cdr lst))
                (list (reverse-all (car lst)))))))
"P.502"
(reverse-all '(((1 2) 3) 4))
(reverse '(((1 2) 3) 4))

"C.3.4 リストの平坦化"
(define flatten
  (lambda (lst)
    (cond ((null? lst) '())
          ((pair? lst)
           (append (flatten (car lst))
                   (flatten (cdr lst))))
          (else (list lst)))))
(flatten '((s1 s2) s1 s2))
(equal? (flatten '(((()) ()) (()) ())) '())

"P.503"
(flatten '(0 (1)))
"P.504"
(equal? (null? '()) #t)
(equal? (null? '(1)) #f)
(list? '())
(pair? '())
(equal? (list? '(a . b)) #f)
(equal? (list? '(a b)) #t)
(pair? '(a . b))
(pair? '(a b))

"P.505"
(append (cons 0 (list))
        (cons (cons 1 (list))
              (list)))
(define flat-in
  (lambda (lst)
    (if (null? lst)
        '()
        (append (car lst)
                (flat-in (cdr lst))))))
(equal? (flat-in '(((()) ()) (()) ())) '((()) () ()))
