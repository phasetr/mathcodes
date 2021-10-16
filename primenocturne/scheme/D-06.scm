(load "./lib")
"P.654"
"D.6 パスカルの三角形と剰余"
(define (bc n r)
  "二項係数 binomial coefficients"
  (/ (fact n) (* (fact r) (fact (- n r)))))
(let ((N 5))
  (map (lambda (n) (bc N n)) (iota 1 5)))

(define (row n)
  (map (lambda (x) (bc n x)) (iota 0 n)))
(row 10)

(apply + (row 0))
(apply + (row 1))
(apply + (row 2))
(apply + (row 3))

"D.6.1 係数の相互関係"
(define (pascal n)
  (map (lambda (x) (row x)) (iota 0 n)))
(pascal 5)
(output (pascal 5))

"P.655"
"剰余による分類"
(define (row-mod n m)
  (map (lambda (x) (/: x m)) (row n)))
(row-mod 10 2)
(define (pas-mod n m)
  (map (lambda (x) (row-mod x m)) (iota 0 n)))
(output (pas-mod 10 3))

"P.656"
"D.6.2 パスカルの三角に色を塗る"
"剰余と座標のリスト"
(define (tri-n n)
  (let ((y (* -1 n)))
    (map (lambda (x) (list x y)) (iota y n 2))))
(output (map tri-n (iota 0 3)))
(output (map (lambda (x) (row-mod x 2)) (iota 0 3)))

(define (mod-xy n m)
  (map flatten
       (map list (tri-n n) (row-mod n m))))
(output (map (lambda (x) (mod-xy x 2)) (iota 0 3)))
"P.657"
(define (plot-data n m)
  (apply append
         (map (lambda (x) (mod-xy x m)) (iota 0 n))))
(plot-data 2 2)

(define (extreme lst)
  (map (lambda (x) (list (apply min x) (apply max x)))
       (apply map list lst)))
(map list '(1 2 3) '(4 5 6) '(7 8 9))

(extreme (plot-data 63 4))

"P.658"
"D.6.3 描画ソフトへの対応"
(define (key-1 obj lst)
  (let loop ((lst lst) (tmp '()))
    (cond ((null? lst) tmp)
          ((equal? (caddar lst) obj)
           (loop (cdr lst) (cons (car lst) tmp)))
          (else (loop (cdr lst)            tmp)))))
(key-1 0 (plot-data 2 2))

(define (build n m)
  (let* ((lst (plot-data n m))
         (min-v (car  (list-ref (extreme lst) 2)))
         (max-v (cadr (list-ref (extreme lst) 2)))
         (LF '()))
    (let loop ((k min-v) (tmp '()))
      (cond ((< max-v k) (reverse tmp))
            (else
             (loop (++ k)
                   (cons LF (append (key-1 k lst) tmp))))))))
(output (build 63 2))
(save->data 'P659-pascal2.dat (build 63 2))

"P.660"
(define (pas-mat j N)
  (append (map (lambda (i) (bc (+ i j) i)) (iota 0 N))
          '(......)))
(pas-mat 0 9)
(output (map (lambda (x) (pas-mat x 9)) (iota 0 3)))

"P.661"
"D.6.4 係数の分布を調べる"
(output (pas-mod 5 4))
(flatten (pas-mod 5 4))

(define (tri-num k)
  (/ (* (+ k 1) (+ k 2)) 2))

(define (dis-1 k n i)
  "distribution (one coefficient)"
  (list i (length (filter
                   (lambda (x) (= x i))
                   (flatten (pas-mod k n))))))
(define (bc-dis k n)
  (map (lambda (x) (dis-1 k n x))
       (iota 0 (- n 1))))
(bc-dis 5 4)

"P.662"
"分布のグラフ"
(define (bc-ratio k n)
  (let* ((coeff (bc-dis k n))
         (total (tri-num k)))
    (map (lambda (x) (/ (cadr x) total)) coeff)))
(bc-ratio 5 4)

(define (pile lst)
  (let* ((k (- (length lst) 1))
         (total (apply + lst))
         (lst (reverse lst))
         (sum (lambda (x) (/ (apply + x) total))))
    (let loop ((i k) (tmp '()))
      (if (< i 0)
          (map sum (reverse tmp))
          (loop (-- i) (cons (list-tail lst i) tmp))))))
(pile (bc-ratio 5 4))

(define (pie-chart lst)
  (map (lambda (x)
         ;; pi/2 で初期位置を上に設定する
         ;;(list (+ 3/2pi (* 2pi x)) 1))
         ;; 円グラフは (1,0) からはじまる
         (list (* 2pi x) 1))
       (pile lst)))
"P.663"
(save->data 'P663-pie.dat (pie-chart (bc-ratio 5 4)))

"剰余0の場合を調べる"
"P.664"
(define (zero-ratio k m)
  (map (lambda (i)
         (/ (cadar (bc-dis i m))
            (tri-num i)))
       (iota 1 k)))
(define zr-100-2 (zero-ratio 100 2))
(map decimal zr-100-2)
(map
 (lambda (n)
   (save->data
    (string->symbol (string-append "P664-zero-ratio" (number->string n) ".dat"))
    (map exact->inexact (zero-ratio 100 n))))
 (iota 2 5))

"P.665"
(define (variety k j n)
  (map (lambda (i)
         (/ (cadr (dis-1 k i 0))
            (tri-num k)))
       (iota j n)))
(save->data 'P665-variety101.dat
            (map exact->inexact (variety 100 2 101)))

(save->data 'P665-bc-ratio-31.dat
            (map exact->inexact (bc-ratio 100 31)))
(save->data 'P665-bc-ratio-32.dat
            (map exact->inexact (bc-ratio 100 32)))
(save->data 'P665-bc-ratio-63.dat
            (map exact->inexact (bc-ratio 100 63)))
(save->data 'P665-bc-ratio-64.dat
            (map exact->inexact (bc-ratio 100 64)))

"P.667"
"D.6.5 パスカルカラーの世界"
(define (colors x)
  (let ((rgb (lambda (r g b)
               (+ (* 65536 r) (* 256 g) b))))
    (cond ((= x  0) (rgb 255 255   0)) ; yellow
          ((= x  1) (rgb   0   0   0)) ; black
          ((= x  2) (rgb 255 165   0)) ; orange
          ((= x  3) (rgb 154 205  50)) ; yellow green
          ((= x  4) (rgb   0 255   0)) ; green
          ((= x  5) (rgb 173 216 230)) ; light blue
          ((= x  6) (rgb 160  32 240)) ; purple
          ((= x  7) (rgb 255   0   0)) ; red
          ((= x  8) (rgb 165  42  42)) ; brown
          ((= x  9) (rgb   0   0 255)) ; blue
          ((= x 10) (rgb 255 192 203)) ; pink
          ((= x 11) (rgb 255 255 255)) ; white
          (else 'fail))))

"P.668"
(define (pascalcolor n m)
  (map (lambda (x)
         (list (car x)
               (cadr x)
               (colors (caddr x))))
       (plot-data n m)))
(save->data 'P668-pascal-03.dat (pascalcolor 63  3))
(save->data 'P668-pascal-04.dat (pascalcolor 63  4))
(save->data 'P668-pascal-05.dat (pascalcolor 63  5))
(save->data 'P668-pascal-06.dat (pascalcolor 63  6))
(save->data 'P668-pascal-07.dat (pascalcolor 63  7))
(save->data 'P668-pascal-08.dat (pascalcolor 63  8))
(save->data 'P668-pascal-09.dat (pascalcolor 63  9))
(save->data 'P668-pascal-10.dat (pascalcolor 63 10))
(save->data 'P668-pascal-11.dat (pascalcolor 63 11))
(save->data 'P668-pascal-12.dat (pascalcolor 63 12))

"P.670"
"D.6.6 墨絵の世界"
"P.671"
(define (monochrome x)
  (let ((rgb (lambda (r g b)
               (+ (* 65536 r) (* 256 g) b))))
    (cond ((= x  9) (rgb   0   0   0)) ; black
          ((= x  7) (rgb  26  26  26)) ; gray10
          ((= x  3) (rgb  51  51  51)) ; gray20
          ((= x  2) (rgb  77  77  77)) ; gray30
          ((= x  8) (rgb 102 102 102)) ; gray40
          ((= x  4) (rgb 127 127 127)) ; gray50
          ((= x  6) (rgb 153 153 153)) ; gray60
          ((= x  1) (rgb 179 179 179)) ; gray70
          ((= x  5) (rgb 204 204 204)) ; gray80
          ((= x  0) (rgb 229 229 229)) ; gray90
          (else 'fail))))

(define (pascalmono n)
  (map (lambda (x)
         (list (car x)
               (cadr x)
               (monochrome (caddr x))))
       (plot-data n 10)))
(save->data 'P671-pascalmono.dat (pascalmono 63))

"P.673"
"D.6.7 カタラン数と径路"
(define (catalan n)
  (/ (comb (* 2 n) n) (+ n 1)))
(map catalan (iota 0 5))
"P.674"
(define (catalan-rec n)
  (if (= n 0)
      1
      (* (/ (* 2 (- (* 2 n) 1)) (+ n 1))
         (catalan-rec (- n 1)))))
(map catalan-rec (iota 0 13))

(define (catalan-odd n)
  (let loop ((i n) (tmp '()))
    (cond ((= i 0) tmp)
          ((odd? (catalan i))
           (loop (-- i) (cons i tmp)))
          (else (loop (-- i)    tmp)))))
(catalan-odd 1000)

"P.675"
"D.6.8 冪の三角形を作る"
(define (m-bc n r)
  "modified binomial coefficients"
  (cond ((or (= r 0) (= r n)) 1)
        ((< n r) 0)
        (else (+ (m-bc (- n 1) (- r 1))
                 (m-bc (- n 1)    r)))))
(define psc
  ;;"power-sum coefficients"
  (memoize
   (lambda (n r)
     (cond ((= r 1) 1)
           ((= r n) (fact (- n 1)))
           (else (+ (* (- r 1) (psc (- n 1) (- r 1)))
                    (* r       (psc (- n 1)    r))))))))
(define (ps k n)
  (let ((fn (lambda (i)
              (* (psc (+ k 1) i) (m-bc n i)))))
    (apply + (map fn (iota (+ k 1))))))
(map (lambda (x) (ps x 4)) (iota 0 4))

"P.676"
(define (psum k n)
  (apply + (map (lambda (i) (** i k)) (iota n))))

(define (psc-row n)
  (map (lambda (x) (psc n x)) (iota n)))
(psc-row 5)
(define (psc-tri k)
  (map psc-row (iota k)))
(output (psc-tri 9))

"P.678"
(define (scan-y n)
  (let loop ((y 2))
    (if (< 0 (apply * (map (lambda (x) (/@ x y))
                           (flatten (psc-tri n)))))
        y
        (loop (++ y)))))
(map scan-y (iota 50))
