(load "./lib")
"P.733"
"D.11 黄金の花を愛でる"
"P.734"
(define (g-angle n)
  (let* ((num (/ (+ 1 (sqrt 5)) 2))
         (ang (/ (* 2 pi) (+ 1 num)))
         (size 5)
         (k (lambda (x) (** 0.991 x))))
    (map (lambda (t)
           (list (* (k t) (cos (* t ang)))
                 (* (k t) (sin (* t ang)))
                 (* (k t) size)))
         (iota 0 (- n 1)))))
(save->data 'P734-g-angle400.dat (g-angle 400))

"P.735"
(define (helix s k)
  (let ((min (* -1 pi))
        (max pi)
        (step (/ pi 20)))
    (map (lambda (t)
           (list (* t (cos (+ t k)))
                 (* t (sin (+ t k)))))
         (cond ((positive? s) (iota 0 max step))
               ((negative? s) (iota min 0 step))))))
(define (helixes s n)
  (let ((unit(/ (* 2 pi) n)))
    (map (lambda (x) (append (helix s x) '(())))
         (iota 0 (* unit n) unit))))

(define (concat lsts)
  (apply append lsts))
(concat '(((1 2) (3 4)) ((5 6) (7 8))))

(define (sunflower n)
  ;;; (flat-in
  (concat
   (append (helixes 1 (fib n))
           '(())
           (helixes -1 (fib (- n 1))))))
(map
 (lambda (n)
   (let ((fname
          (string->symbol
           (string-append "P735-sunflower" (number->string n) ".dat"))))
     (save->data
      fname
      (sunflower n))))
 (iota 2 10))
"P.736"
(map fib (iota 2 11))
