"P.827"
"D.24 RSA暗号"
"D.24.1 文字と数字の相互変換"
"桁揃ええとencode"
"P.828"
(define (prefix0 x)
  (cond ((equal? x "5") "05")
        ((equal? x "6") "06")
        ((equal? x "7") "07")
        ((equal? x "8") "08")
        ((equal? x "9") "09")
        (else x)))
(define (encode str)
  (string->number
   (apply string-append
          (map prefix0
               (map number->string
                    (shift-encode str -27))))))
(encode "Scheme")
(encode "#Scheme")

"文字分割とdecode"
"P.829"
(define (string-divide s str)
  (let ((len (string-length str)))
    (let loop ((k 0) (tmp '()))
      (cond ((or (<= s 0) (<= len s)) str)
            ((= k len) (reverse tmp))
            ((< len (+ k s))
             (reverse (cons (substring str k len) tmp)))
            (else (loop (+ k s)
                        (cons (substring str k (+ k s)) tmp)))))))
(string-divide 0 "abcdefghijklmn")
(string-divide 1 "abcdefghijklmn")
(string-divide 2 "abcdefghijklmn")
(string-divide 6 "abcdefghijklmn")

"P.830"
(define (decode num)
  (let ((str (number->string num)))
    (shift-decode
     (map (lambda (x) (+ 27 x))
          (map string->number
               (string-divide 2
                              (if (odd? (string-length str))
                                  (string-append "0" str)
                                  str))))
     0)))
(let ((str (number->string 567277748274)))
  (map string->number
               (string-divide 2
                              (if (odd? (string-length str))
                                  (string-append "0" str)
                                  str))))

(decode 567277748274)
(decode 8567277748274)

"D.24.2 暗号生成と暗号解読"
(map-encode "Go/No-Go" 0)
(/@ (** 7111147781114571111 5) 7000000013390000000171)
(factorize-of 7000000012680000000144)
(gcd 7000000012680000000144 5)

(euclid-ss 5 -7000000012680000000144)

"P.832"
(/:** 2573486531281732474725
      1400000002536000000029
      7000000013390000000171)

(define (RSA-encode str)
  (let ((x (encode str))
        (n 180026617919604454524900343585335434461)
        (r 65537))
    (/:** x r n)))
(define (RSA-decode num)
  (let* ((p 13265535618784201153)
         (q 13571002565828100637)
         (phi (* (- p 1) (- q 1)))
         (r 65537)
         (s (cadr (euclid-ss r (- phi)))))
    (decode (/:** num s (* p q)))))
(RSA-encode "We go for launch")
(RSA-decode (RSA-encode "We go for launch"))
