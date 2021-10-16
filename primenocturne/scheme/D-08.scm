(load "./lib")
"P.687"
"D.8 ベクトルの変換性"
"D.8.1 二つの積と行列式"
"P.688"
"D.8.2 和の規約と縮約計算"
"P.692"
(define (mag vec)
  (sqrt (mat* vec vec)))
(define (ang-dot vec1 vec2)
  (let ((cos-ang (/ (mat* vec1 vec2)
                    (mag vec1)
                    (mag vec2))))
    (acos cos-ang)))

(define e1 '(1 0 0))
(define e2 '(0 1 0))
(define e3 '(0 0 1))
(mag e1)
(mag e2)
(mag e3)

(ang-dot e1 e1)
(ang-dot e1 e2)
(ang-dot e1 e3)
(ang-dot e2 e1)
(ang-dot e2 e2)
(ang-dot e2 e3)
(ang-dot e3 e1)
(ang-dot e3 e2)
(ang-dot e3 e3)

"P.693"
(define (cross vec)
  (let ((x (car   vec))
        (y (cadr  vec))
        (z (caddr vec)))
    (list (list 0     (- z)    y)
          (list z        0  (- x))
          (list (- y)    x     0))))

(mat* e1 (cross e1))
(mat* e1 (cross e2))
(mat* e1 (cross e3))
(mat* e2 (cross e1))
(mat* e2 (cross e2))
(mat* e2 (cross e3))
(mat* e3 (cross e1))
(mat* e3 (cross e2))
(mat* e3 (cross e3))

(define (ang-cross vec1 vec2)
  (let* ((vxv (mat* vec1 (cross vec2)))
         (sin-ang (/ (sqrt (mat* vxv vxv))
                     (mag vec1)
                     (mag vec2))))
    (asin sin-ang)))
(ang-cross e1 e1)
(ang-cross e1 e2)
(ang-cross e1 e3)

"P.694"
"D.8.3 ベクトルの展開"

"P.698"
"D.8.4 座標系の回転とベクトルの回転"

"P.700"
"D.8.5 三次元の回転"
