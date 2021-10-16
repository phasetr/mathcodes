(load "./lib")
"P.702"
"D.9 行列で蝶を愛でる"
"D.9.1 回転行列と正多角形"
(define (rot-1 t)
  (let ((c (cos t)) (s (sin t)))
    (list (list c (* -1 s)) (list s c))))
(define (polygon n)
  (define (points n)
    (let ((v0 '((1) (0))))
      (map mat-t
           (append
            (map (lambda (x)
                   (mat* (mat** (rot-1 (/ 2pi n)) x) v0))
                 (iota 0 (- n 1)))
            (list v0)))))
  (dismap adjust-of (points n)))

(polygon 3)
(polygon 4)
(polygon 5)
(polygon 6)

(save->data 'P703-polygon3.dat (polygon 3))
(save->data 'P703-polygon4.dat (polygon 4))
(save->data 'P703-polygon5.dat (polygon 5))
(save->data 'P703-polygon6.dat (polygon 6))

"P.704"
"D.9.2 不可能を描く"
"impossible-triangle"
(define it2
  (let* ((S 2.0)
         (k (/ (sqrt 3) 2))
         (x0 (/ S 2))
         (y0 (* x0 k))
         (L (/ S 8))
         (H (* L k))
         (@ (lambda (x y)
              (list (- (* L x) x0) (- (* H y) y0))))
         (p1  (@ 1.0 0)) (p2  (@ 7.0 0)) (p3  (@ 7.5 1))
         (p4  (@ 4.5 7)) (p5  (@ 3.5 7)) (p6  (@ 0.5 1))
         (p7  (@ 4.5 5)) (p8  (@ 3.0 2)) (p9  (@ 2.0 2))
         (p10 (@ 5.0 2)) (p11 (@ 5.5 1)) (p12 (@ 4.0 4)))
    (list p1 p2 p3 p4 p5 p6 p1
          p2 p7 p8 p9 p4 p9 p10 p12 p11 p6)))
"P.705"
(save->data 'P705-it2.dat it2)

(define (data-rot proc degree)
  (let ((k (* (/ pi 180) degree)))
    (map mat-t (map (lambda (x) (mat* (rot-1 k) x))
                    (map mat-t proc)))))
(save->data 'P705-it2-rot.dat (data-rot it2 45))

"P.706"
(define (butterfly t)
  (let* ((r (+ (exp (sin t))
               (* -2 (cos (* 4 t)))
               (** (sin (/ t 12)) 5)))
         (x (* r (cos t)))
         (y (* r (sin t)))
         (pi (* 4 (atan 1))))
    (list x y)))
(save->data 'P706-texsq2.dat
            (map butterfly (iota 0 (* 24 pi) (/ pi 240))))

(map
 (lambda (n)
   (save->data
    (string->symbol (string-append "P706-texsq-many-" (number->string n) ".dat"))
    (map butterfly (iota 0 (* n pi) (/ pi (* n 10))))))
 (iota 1 24))

"P.708"
"D.9.3 三次元への飛翔"
(define it3
  (map (lambda (x) (append x (list 0))) it2))
(save->data 'P708-it3.dat it3)

(define (spiral n)
  (let* ((pi (* 4 (atan 1)))
         (k (/ pi 240)))
    (map (lambda (x) (list (cos x) (sin x) x))
         (iota 0 (* 2 pi n) k))))
(save->data 'P708-spiral.dat (spiral 1))

(define (spiral2 n)
  (let* ((pi (* 4 (atan 1)))
         (k (/ pi 240))
         (cosin (lambda (t) (real-part (exp (* +i t)))))
         (sine  (lambda (t) (imag-part (exp (* +i t))))))
    (map (lambda (x) (list (cosin x) (sin x) x))
         (iota 0 (* 2 pi n) k))))
(save->data 'P708-spiral.tmp.dat (spiral2 1))

"P.711"
(define (rot-e3 p)
  (let ((c3 (cos p))
        (s3 (sin p)))
    (list (list c3 (* -1 s3) 0)
          (list s3 c3        0)
          (list 0  0         1))))
(define (rot-e2 t)
  (let ((c2 (cos t))
        (s2 (sin t)))
    (list (list  c2       0 s2)
          (list  0        1  0)
          (list (* -1 s2) 0 c2))))
(define (rot-3d psi theta phi)
  (mat* (rot-e3 psi)
        (mat* (rot-e2 theta) (rot-e3 phi))))
(define (data-r3d proc psi theta phi)
  (let* ((rad   (/ pi 180))
         (psi   (* rad psi))
         (theta (* rad theta))
         (phi   (* rad phi)))
    (map mat-t
         (map (lambda (x)
                (mat* (rot-3d psi theta phi) x))
              (map mat-t proc)))))
(define texsq3
  (map (lambda (x) (append x (list 0)))
       (map butterfly (iota 0 (* 24 pi) (/ pi 240)))))
(save->data 'P711-texsq3-rot.dat
            (data-r3d texsq3 60 -15 -15))
