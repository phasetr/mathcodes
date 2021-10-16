(load "./lib")
"P.820"
"D.22 拡張された互除法"
"D.21 互除法における値の変化"
(define (my-gcd s r)
  (if (zero? r)
      s
      (my-gcd r (/@ s r))))

"P.821"
(define (euclid-ss a b)
  ;;;"special solution"
  (define (ext-ss s r sa sb ra rb)
    (let ((trans
           (lambda (x y u v) (- x (* y (// u v))))))
      (if (zero? r)
          (list 'x= sa 'y= sb 'gcd s)
          (ext-ss r (/@ s r)
                  ra rb
                  (trans sa ra s r)
                  (trans sb rb s r)))))
  (ext-ss a b 1 0 0 1))
"P.822"
(euclid-ss 143 195)
(euclid-ss 11 15)

"D.22.2 一般解を導く"
(define (euclid-gs a b c)
  ;;; general solution
  (define (ext-gs s r c sa sb ra rb)
    (let ((trans
           (lambda (x y u v) (- x (* y (// u v))))))
      (if (zero? r)
          (if (not (zero? (/@ c s)))
              'No-solution
              (list (list 'x= (* sa (/ c s)) '+ (/ b s) 't)
                    (list 'y= (* sb (/ c s)) '- (/ a s) 't)))
          (ext-gs r (/@ s r) c
                  ra rb
                  (trans sa ra s r)
                  (trans sb rb s r)))))
  (ext-gs a b c 1 0 0 1))
(euclid-gs 143 195 2)
"P.823"
(euclid-gs 143 195 13)
(euclid-gs 143 195 26)
