;;; # Special Pythagorean triplet
;;; - [URL](https://projecteuler.net/problem=9)
;;; ## Problem 9
;;; A Pythagorean triplet is a set of three natural numbers, a < b < c, for which,
;;; $a^2 + b^2 = c^2$.
;;; For example, 32 + 42 = 9 + 16 = 25 = 52.
;;; There exists exactly one Pythagorean triplet for which a + b + c = 1000.
;;; Find the product abc.
;;;
;;; ピタゴラスの三つ組みは $a < b < c$ かつ $a^2 + b^2 = c^2$ をみたす 3 つの自然数の集合を指す。
;;; ここで $a+b+c=1000$$ をみたす三つ組みはただ1つしかない。
;;; このときの積 $abc$ を求めよ。
(defun solve (n)
  (let ((c 0))
    (remove-if-not
     #'(lambda (x) x)
     (loop for a from 1 to n
           append (loop for b from a to n
                        collect
                        (progn
                          (setq c (- n a b))
                          (format nil "~a" c)
                          (if (= (* c c) (+ (* a a) (* b b)))
                              (* a b c)
                              'nil)))))))
(solve 1000)
