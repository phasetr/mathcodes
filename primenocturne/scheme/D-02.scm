(load "./lib")

"P.590"
"D.2 フィボナッチ数列に学ぶ"
(define (fib n)
  (cond ((= n 0) 0)
        ((= n 1) 1)
        (else (+ (fib (- n 1)) (fib (- n 2))))))
(map fib (iota 16))

"D.2.1 再帰の動き"
"P.591"
"末尾再帰への準備"
"P.592"
(define (fib n)
  (define (fib-iter s1 s2 i)
    (if (zero? i)
        s2
        (fib-iter (+ s1 s2) s1 (-- i))))
  (fib-iter 1 0 n))
(map fib (iota 50))

"P.593"
(assoc '(fib 3) '(((fib 1) 1) ((fib 2) 1) ((fib 3) 2)))
;;;(define (assoc key data)
;;;  (cond ((null? data) #f)
;;;        ((equal? key (caar data)) (car data))
;;;        (else (assoc key (cdr data)))))

"P.594"
(define (memoize proc)
  (let ((table '()))
    (lambda x
      (let ((stock (assoc x table)))
        (if stock
            (cdr stock)
            (let ((data (apply proc x)))
              (set! table
                    (cons (cons x data) table)) data))))))
(define fib-memo
  (memoize (lambda (n)
             (cond ((= n 0) 0)
                   ((= n 1) 1)
                   (else (+ (fib-memo (- n 1))
                            (fib-memo (- n 2))))))))
(map fib-memo (iota 50))

"P.596"
"D.2.2 一般項の計算"
(define (fib n)
  (let ((g (/ (+ 1 (sqrt 5)) 2)))
    (inexact->exact
     (round
      (/ (- (** g n) (** (- 1 g) n)) (sqrt 5))))))
"P.597"
"無理数の除去"
(fib 4)
(fib 16)
(map fib (iota 10))

"P.598"
"quadratic irrational"
(define (qi5 n)
  (let* ((h 1/2) (a h) (b h))
    (let loop ((i 1) (x h) (y h))
      (if (= i n)
          (list x y 'q= (+ x (* y (sqrt 5))))
          (loop (++ i)
                (+ (* a x) (* 5 b y))
                (+ (* a x) (*   b y)))))))
(qi5 1)
(qi5 2)
(define (fib-q5 n)
  (* 2 (cadr (qi5 n))))
(map fib-q5 (iota 16))

"D.2.3 行列計算の為の手続"
"P.599"
(define v '(1 0))
(define m '((1 1) (1 0)))
"数ベクトルへの対応"
(define v-t '((1) (0)))
"P.600"
(define (rowvec? x)
  "単純なリスト形式ならば行ベクトル"
  (and (list? x) (number? (car x))))
(define (colvec? x)
  "先頭に一要素だけを含むリストを持つ二重リストは列ベクトル"
  (and (list? x) (list? (car x))
       (= 1 (length (car x)))))
(define (mat? x)
  "先頭に二要素以上を含む二重リストは行列"
  (and (list? x) (list? (car x))
       (< 1 (length (car x)))))

(define (mat-t mat)
  (define (mat-form mat)
    (map (lambda (y) (map (lambda (x) (list-ref x y)) mat))
         (iota 0 (- (length (car mat)) 1))))
  (define (vec-form mat)
    (if (list? (car mat))
        (flatten mat)
        (map (lambda (x) (list (list-ref mat x)))
             (iota 0 (- (length mat) 1)))))
  (cond ((mat? mat) (mat-form mat))
        (else (vec-form mat))))

(mat-t '((2 3) (5 7)))
(mat-t '((1) (2)))
(mat-t (mat-t '((1) (2))))

"P.601"
(define (dot* v1 v2)
  (cond ((and (number? v1) (number? v2)) (* v1 v2))
        ((or  (number? v1) (number? v2)) (sca*vec v1 v2))
        ((and (colvec? v1) (rowvec? v2)) (col*row v1 v2))
        ((and (rowvec? v1) (colvec? v2)) (row*col v1 v2))
        (else (row*col v1  (mat-t   v2)))))
(define (sca*vec sca vec)
  (let* ((dummy sca)
         (judge? (and (number? sca) (list? vec)))
         (sca (if judge? sca vec))
         (vec (if judge? vec dummy))
         (calc1 (lambda (x y)
                  (map (lambda (t) (* x t)) y)))
         (calc2 (lambda (x y)
                  (map (lambda (t) (list (* x (car t)))) y))))
    (if (number? (car vec))
        (calc1 sca vec)
        (calc2 sca vec))))
(define (col*row v1 v2)
  (define (row-calc lst)
    (map (lambda (x) (* (car lst) x)) (cadr lst)))
  (map row-calc
       (map (lambda (x) (list x v2)) (mat-t v1))))
(define (row*col v1 v2)
  (apply + (map * v1 (flatten v2))))

;;; 行ベクトル
(define A '(2 3 5))
(equal? (rowvec? A) #t)
;;; 列ベクトル
(define B '((2) (3) (5)))
(equal? (colvec? B) #t)

(equal? (dot* 2 3) 6)
(equal? (dot* 2 A) '(4 6 10))
"P.602"
(equal? (dot* A 2) '(4 6 10))
(equal? (dot* 2 B) '((4) (6) (10)))
(equal? (dot* B 2) '((4) (6) (10)))
(equal? (dot* A (mat-t A)) 38)
(equal? (dot* (mat-t B) B) 38)
(equal? (dot* B A) '((4 6 10) (6 9 15) (10 15 25)))
(equal? (dot* A B) 38)

"積の等号手続と逆行列"
(define (mat* m1 m2)
  (cond ((and (mat?    m1) (mat?    m2)) (mat*mat m1 m2))
        ((and (rowvec? m1) (mat?    m2)) (vec*mat m1 m2))
        ((and (mat?    m1) (colvec? m2)) (mat*vec m1 m2))
        ((or (number?  m1) (number? m2)) (sca*mat m1 m2))
        (else (dot* m1 m2))))
(define (mat*mat m1 m2)
  (let ((columns (mat-t m2)))
    (map (lambda (x)
           (map (lambda (y) (dot* x y)) columns)) m1)))
(define (vec*mat vec mat)
  (map (lambda (x) (dot* vec x)) (mat-t mat)))
(define (mat*vec mat vec)
  (mat-t (map (lambda (x) (dot* vec x)) mat)))
(define (sca*mat sca mat)
  (let* ((dummy sca)
         (judge? (and (number? sca) (list? mat)))
         (sca (if judge? sca mat))
         (mat (if judge? mat dummy)))
    (map (lambda (x) (dot* sca x)) mat)))

"P.603"
(define (mat+ m1 m2)
  (cond ((and (mat? m1) (mat? m2)) (map vec+ m1 m2))
        (else (vec+ m1 m2))))
(define (mat- m1 m2) (mat+ m1 (mat* -1 m2)))
(define (vec+ v1 v2)
  (let ((calc (lambda (x y)
                (map (lambda (t) (apply + t))
                     (map list x y)))))
    (cond ((and (rowvec? v1) (rowvec? v2)) (calc v1 v2))
          ((and (colvec? v1) (colvec? v2))
           (mat-t (calc (mat-t v1) (mat-t v2)))))))

"P.604"
(define (inv2 mat)
  (let* ((a (caar  mat)) (b (cadar  mat))
         (c (caadr mat)) (d (cadadr mat))
         (det2 (- (* a d) (* b c))))
    (if (zero? det2)
        'impossible
        (list (list    (/ d det2)  (- (/ b det2)))
              (list (- (/ c det2))    (/ a det2))))))
(inv2 m)
(mat* m (inv2 m))

"P.605"
(define (inv3 mat)
  (let* ((det2 (lambda (a b c d) (- (* a d) (* b c))))
         (a (lambda (i j)
              (list-ref (list-ref mat (- i 1)) (- j 1))))
         (a11 (a 1 1)) (a12 (a 1 2)) (a13 (a 1 3))
         (a21 (a 2 1)) (a22 (a 2 2)) (a23 (a 2 3))
         (a31 (a 3 1)) (a32 (a 3 2)) (a33 (a 3 3))
         (m11 (* +1 (det2 a22 a23 a32 a33)))
         (m12 (* -1 (det2 a21 a23 a31 a33)))
         (m13 (* +1 (det2 a21 a22 a31 a32)))
         (m21 (* -1 (det2 a12 a13 a32 a33)))
         (m22 (* +1 (det2 a11 a13 a31 a33)))
         (m23 (* -1 (det2 a11 a12 a31 a32)))
         (m31 (* +1 (det2 a12 a13 a22 a23)))
         (m32 (* -1 (det2 a11 a13 a21 a23)))
         (m33 (* +1 (det2 a11 a12 a21 a22)))
         (det3 (+ (* a11 m11) (* a12 m12) (* a13 m13)))
         (cof (list (list m11 m12 m13)
                    (list m21 m22 m23)
                    (list m31 m32 m33))))
    (mat* (/ 1 det3) (mat-t cof))))
(define C '((3 4 5) (1 2 3) (2 -5 4)))
(inv3 C)
"P.606"
(mat* C (inv3 C))

(define (einheit n)
  (let ((lst (iota n)))
    (map (lambda (i)
           (map (lambda (x) (if (= x i) 1 0)) lst))
         lst)))
(einheit 4)

"実行例から行列の性質を知る"
(define A '((1 0) (0 0)))
(define B '((0 -1) (1 0)))
(mat* A B)
(mat* B A)
(mat* A A)
(mat* B B)
(let ((C (mat* A B)))
  (mat* C C))
(mat* (mat* A A) (mat* B B))

"P.607"
(inv2 A)
(inv2 B)
(mat* (inv2 B) B)
(mat* B (inv2 B))

(define I '((0 1) (-1 0)))
(mat* I I)

(define U '((2  -3 -5) (-1 4  5) (1  -3 -4)))
(define V '((-1  3  5) (1 -3 -5) (-1  3  5)))
(define W '((2  -2 -4) (-1 3  4) (1  -2 -3)))
(mat* U V)
(mat* V W)
(mat* U W)
(mat* W U)
(mat* (mat* U V) W)
(mat* (mat* W V) U)

(define N3 '((1 1 3) (5 2 6) (-2 -1 -3)))
(mat* N3 N3)
(mat* (mat* N3 N3) N3)

"D.2.4 行列の逐次平方"
(define (mat** sm n)
  (cond ((= n 0) (einheit (length sm)))
        ((= n 1) (sm))
        (else (mat*mat sm (mat** sm (- n 1))))))

(define (mat** sm n)
  (let ((mat-sq (lambda (x) (mat*mat x x))))
    (cond ((= n 0) (einheit (length sm)))
          ((= n 1) sm)
          ((even? n) (mat-sq     (mat** sm (/ n 2))))
          ((odd?  n) (mat*mat sm (mat** sm (- n 1)))))))
(mat** N3 0)
(mat** N3 1)
(mat** N3 2)
(mat** N3 3)

(define (fib-mat n)
  (let ((n (- n 1)) (m '((1 1) (1 0))))
    (if (= n 0)
        1
        (caar (mat** m n)))))
(map fib-mat (iota 10))

(define (fib-pq n)
  (let loop ((a 1) (b 0) (p 0) (q 1) (i n))
    (cond ((= i 0) b)
          ((even? i)
           (loop a b (+ (* p p) (* q q))
                 (+ (* 2 p q) (* q q)) (/ i 2)))
          (else (loop (+ (* p a) (* q a) (* q b))
                      (+ (* q a) (* p b)) p q (-- i))))))
(map fib-pq (iota 10))

"P.611"
"D.2.5 ベクタとリスト"
(make-vector 5 'a)
(vector 2 3 5 7 11)

(define v5 #(2 3 5 7 11))

(vector-length v5)
(vector-ref v5 3)
(vector->list v5)
(list->vector '(2 3 5 7 11))

(vector-set! v5 3 0)
v5

(define v5 #(2 3 5 7 11))
(equal? (list? (vector->list v5))   #t)
(equal? (vector? (vector->list v5)) #f)
(equal? (vector? v5) #t)
