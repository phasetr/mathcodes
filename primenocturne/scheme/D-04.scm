(load "./lib")
"P.629"
"D.4 素数を求める"
"D.4.1 チューリングマシン"

"P.630"
"D.4.2 終了条件"
"事前の準備"
"P.631"
(define (add-mark lst k)
  (let loop ((i k) (ones '()))
    (if (zero? i)
        (append lst ones)
        (loop (-- i) (cons '-1 ones)))))
(define nn99 (iota 99))
(define nn20 (iota 20))
(add-mark nn99 2)
(add-mark (iota 20) 2)

"D.4.3 更新と書込み"
"P.632"
"更新の実現"
(define (nn-tail lst i) (list-tail lst (- i 1)))
(define (nn-ref  lst i) (list-ref  lst (- i 1)))
(nn-tail nn99 91)
(nn-ref  nn99 91)

(define (zero->nn lst k)
  (set-car! (nn-tail lst k) 0))
(zero->nn nn99 4) ; nn99 が破壊的に変更されている

(define (jump-p lst p)
  (let loop ((i p))
    (cond ((= (nn-ref lst i) -1) (filter del-mark? lst))
          ((= i p)               (loop (+ i p)))
          (else (zero->nn lst i) (loop (+ i p))))))
(define (del-mark? x) (<= 0 x))
"P.633"
(define (ps lst p)
  (jump-p (add-mark lst p) p))

(define nn99 (iota 99))
(ps nn99 2)

"原点復帰"
(ps (ps nn99 2) 3)
(list-tail (ps (ps nn99 2) 3) 90)
(ps (ps nn20 2) 3)

"D.4.4 探索範囲の設定"
"P.634"
"無駄除去機能"
(define (scan lst)
  (let loop ((i 1) (tmp lst))
    (cond ((range? lst i) tmp)
          ((zero? (nn-ref tmp (++ i))) (loop (++ i) tmp))
          (else (loop (++ i) (ps tmp (++ i)))))))
(define (range1? lst i) (= (length lst) i))
(define (range2? lst i) (= (/ (length lst) 2) i))
(define (range? lst i) (< (length lst) (* i i)))
(define (primes n)
  (filter (lambda (x) (< 1 x))
          (scan (iota n))))
(primes 100)

"P.635"
"Elephant なコード"
(define (add-mark lst)
  (let loop ((i 0) (ones '()))
    (if (range? lst i)
        (append lst ones)
        (loop (++ i) (cons '-1 ones)))))
(define (ps lst p)
  (jump-p (add-mark lst) p))
(primes 10000)

"P.636"
"余談: 小学生にもチューリングを!"

"P.638"
"D.4.5 エラトステネスの篩と素数分布"
"計算による方法"
(define (prime? n)
  (define (search n)
    (let loop ((i 2))
      (cond ((< n (* i i)) n)
            ((zero? (/@ n i)) i)
            (else (loop (++ i))))))
  (if (= n 1)
      #f
      (= n (search n))))
(prime? 4)
(prime? 97)

(define (sieve n)
  (map (lambda (x) (if (prime? x) x 0))
       (iota 2 n)))
"P.639"
(sieve 31)
(define (primes n)
  (filter positive? (sieve n)))
(primes 100)

"集団としての素数"
(primes 99)

"P.640"
(length (primes 9))
(length (primes 99))
(length (primes 999))
(length (primes 9999))

(define (p-dis lst nn)
  "prime distribution"
  (let loop ((i nn) (tmp '()))
    (if (zero? i)
        tmp
        (loop (-- i)
              (cons (length (filter (lambda (x) (<= x i)) lst))
                    tmp)))))
(define pn99 (primes 99))
(p-dis pn99 99)

(define (output lst)
  (let loop ((i 0))
    (newline)
    (if (= i (length lst))
        'end
        (begin (display (list-ref lst i))
               (loop (++ i))))))
(output (p-dis pn99 99))

"P.641"
"D.4.6 データの入出力とグラフ"
"ファイルとポート"
"P.642"
(define call/out call-with-output-file)
(define call/in  call-with-input-file)

(define (call/out name proc)
  (let ((x (open-output-file name)))
    (let ((result (proc x)))
      (close-output-port x) result)))
(define (call/in name proc)
  (let ((x (open-input-file name)))
    (let ((result (proc x)))
      (close-input-port x) result)))

"P.643"
"入出力の手続"
(define (status name)
  (string-append "./data/" (symbol->string name)))
(define (save->file name proc)
  (call/out (status name) (lambda (x) (write proc x))) 'ok)
(save->file 'list-prime.tmp.dat (primes 100))

"P.644"
(define (save->data name proc . opt)
  (define (nl-add lst x)
    "newline add"
    (let* ((fn0 (lambda (k) (list-ref lst k)))
           (fn1 (lambda (y) (write y x)
                        (write-char #\space x)))
           (store (lambda (z)
                    (if (and (null? opt) (list? (fn0 z)))
                        (map fn1 (fn0 z))
                        (write   (fn0 z) x)))))
      (let loop ((i 0))
        (cond ((= i (length lst)) 'ok)
              (else (store i) (newline x) (loop (++ i)))))))
  (call/out (status name) (lambda (x) (nl-add proc x))))
(save->data 'line-prime.tmp.dat (primes 100))

(define (data->load name)
  (call/in (status name)
           (lambda (x)
             (let loop ((dat (read x)) (tmp '()))
               (if (eof-object? dat)
                   (reverse tmp)
                   (loop (read x) (cons dat tmp)))))))
"P.645"
(define (list->load name)
  (call/in (status name) (lambda (x) (write (read x)))))

(define data '())
(define (list->args name)
  (set! data
        (call/in (status name) (lambda (x) (read x)))))

(save->data 'P645-p-dis999.dat (p-dis (primes 999) 999))

"P.647"
"D.4.7 素数と巡回数"
(define (cyclic lst)
  "2, 5 を除いた素数のリストを与える前提で作っている"
  (let loop ((i (- (length lst) 1)) (tmp '()))
    (if (< i 0)
        tmp
        (loop (-- i) (cons (/// 1 (list-ref lst i)) tmp)))))
(define (cyclic-n lst)
  (define (p-n i) (list-ref lst i))
  (define (c-n i) (car (list-ref (cyclic lst) i)))
  (define (k-n i) (/ (- (p-n i) 1) (c-n i)))
  (let loop ((i (- (length lst) 1)) (tmp '()))
    (if (< i 0)
        tmp
        (loop (-- i)
              (cons (list (p-n i) (c-n i) (k-n i))
                    tmp)))))

(define pn99- (cons 3 (list-tail (primes 99) 3)))
(output (cyclic-n pn99-))
