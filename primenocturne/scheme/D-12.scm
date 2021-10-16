(load "./lib")
"P.737"
"D.12 無理数の視覚化"
(define (repdec proc n)
  "repted decimal"
  (let* ((num (/ (+ 1 (sqrt 5)) 2))
         (ang (/ (* 2 pi) (+ 1 num)))
         (size 1)
         (k (lambda (x) (* 10 (** 0.998 x)))))
    (map (lambda (t)
           (list (* (k t) (cos (* t ang)))
                 (* (k t) (sin (* t ang)))
                 (* (k t) size)
                 (colors (proc t))))
         (iota 0 (- n 1)))))
(map (lambda (x) (//check 1 7 x)) (iota 0 6))

"P.738"
(/ 1234567890 9999999999.0)

(save->data
 'P738-repdec.dat
 (repdec (lambda (x) (//check 1234567890 9999999999 x))
         100))

(save->data
 'P738-sample-1-7.dat
 (repdec (lambda (x) (//check 1 7 x))
         2500))
(save->data
 'P738-sample-1-11.dat
 (repdec (lambda (x) (//check 1 11 x))
         2500))
(save->data
 'P738-sample-1-13.dat
 (repdec (lambda (x) (//check 1 13 x))
         2500))
(save->data
 'P738-sample-1-17.dat
 (repdec (lambda (x) (//check 1 17 x))
         2500))
(save->data
 'P738-sample-1-97.dat
 (repdec (lambda (x) (//check 1 97 x))
         2500))
(save->data
 'P738-sample-1-2539.dat
 (repdec (lambda (x) (//check 1 2539 x))
         2500))

"P.739"
(save->data
 'P739-sample-22-7.dat
 (repdec (lambda (x) (//check 7 22 x))
         2500))
(save->data
 'P739-sample-333-106.dat
 (repdec (lambda (x) (//check 333 106 x))
         2500))
(save->data
 'P739-sample-355-113.dat
 (repdec (lambda (x) (//check 355 113 x))
         2500))

"P.740"
(define (irr-stg n)
  "irrational to string"
  (let loop ((i 1)
             (tmp "0"))
    (if (< n i)
        tmp
        (loop (++ i)
              (string-append tmp (number->string i))))))
(irr-stg 9)
(irr-stg 869)
(define (nth869 k)
  (- (char->integer (string-ref (irr-stg 869) k)) 48))
(save->data 'P740-irr869.dat (repdec nth869 2500))
