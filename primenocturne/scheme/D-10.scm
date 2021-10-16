(load "./lib")
"P.713"
"D.10 四元数による回転の記述"
"P.714"
"D.10.1 四元数の性質"
"P.719"
"D.10.2 虚空間の回転角"
"P.725"
"D.10.3 行列による四元数"
(define I '((0 -1  0  0) (1 0  0 0) (0 0 0 -1) (0  0 1 0)))
(define J '((0  0 -1  0) (0 0  0 1) (1 0 0  0) (0 -1 0 0)))
(define K '((0  0  0 -1) (0 0 -1 0) (0 1 0  0) (1  0 0 0)))
(define E (einheit 4))
(define -E (mat* -1 E))

(equal? (mat* I I) -E)
(equal? (mat* J J) -E)
(equal? (mat* K K) -E)
(mat* I J)
(mat* J I)
(mat* J K)
(mat* K J)
(mat* K I)
(mat* I K)
(and (equal? (mat* I J) (mat* -1 (mat* J I))) (equal? (mat* I J) K))
(and (equal? (mat* J K) (mat* -1 (mat* K J))) (equal? (mat* J K) I))
(and (equal? (mat* K I) (mat* -1 (mat* I K))) (equal? (mat* K I) J))
(equal? (mat* I (mat* J K)) -E)

"P.726"
(define (Q w x y z)
  (let ((E (einheit 4)))
    (mat+ (mat* w E)
          (mat+ (mat* x I)
                (mat+ (mat* y J)
                      (mat* z K))))))
(Q 2 3 5 7)
(Q 2 -3 -5 -7)
(mat* (Q 2  3  5  7)
      (Q 2 -3 -5 -7))
(mat* (Q 2 -3 -5 -7)
      (Q 2  3  5  7))
(car (Q 2 3 5 7))
(car (cdddar (Q 2 3 5 7)))
(define (Q->vec3 Q)
  (let ((x (- (cadar   Q)))
        (y (- (caddar  Q)))
        (z (- (car (cdddar Q)))))
    (list (list x) (list y) (list z))))
(Q->vec3 (Q 2 3 5 7))

(define (Qz phi)
  (let ((phi (* phi (/ pi 180)))
        (phihalf (/ phi 2)))
    (Q (cos (/ phi 2)) 0 0 (sin (/ phi 2)))))

"P.727"
(define Rp (Q 0 1 0 0))
(dismap adjust-of
        (mat* (Qz 90) (mat* Rp (Qz -90))))

(define (Qx psi)
  (let ((psi (* psi (/ pi 180))))
    (Q (cos (/ psi 2)) (sin (/ psi 2)) 0 0)))
(define (Qy theta)
  (let ((theta (* theta (/ pi 180))))
    (Q (cos (/ theta 2)) 0 (sin (/ theta 2)) 0)))

"P728"
(dismap
 adjust-of
 (mat* (Qx 90)
       (mat* (mat* (Qy 90)
                   (mat* (mat* (Qz 90) (mat* Rp (Qz -90)))
                         (Qy -90)))
             (Qx -90))))

(Q->vec3
 (dismap adjust-of
         (mat* (Qx 90)
               (mat* (Qy 90) (Qz 90)))))
(Q->vec3
 (dismap adjust-of
         (mat* (Q 0.0 0.707 0.0 0.707)
               (mat* Rp (Q 0.0 -0.707 0.0 -0.707)))))

"D.10.4 四元数による補間"
"線型補間"
"球面線型補間"
