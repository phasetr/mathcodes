// # Multiples of 3 and 5
// - [URL](https://projecteuler.net/problem=1)
// ## Problem 1
// If we list all the natural numbers below 10 that are multiples of 3 or 5, we get 3, 5, 6 and 9. The sum of these multiples is 23.
// Find the sum of all the multiples of 3 or 5 below 1000.
//
// 1000 未満の 3 と 5 の倍数のすべての和を計算せよ。
(defun test ()
  (format t "~a" 3)
  (let* ((x 2))
    (format t "~a" x))
  (if (3or5p 4)
      1
      2)
  (when (3or5p 4)
    1)
  (let ((x 4))
    (if (oddp x)
        (1+ x)
        x))
  (remove-if-not #'(lambda (x) (if (oddp x) (1+ x)))
                 '(1 2 3 4))
  (remove-if-not #'3or5p
                 '(1 2 3 4 5 6 7 8 9 10))
  (reduce #'+ '(1 2))
  (reduce #'+ (remove-if-not #'3or5p
                             '(1 2 3 4 5 6 7 8 9 10)))
  (loop :for i :below 1000 :collect i))

(defun 3or5p (x)
  (cond
    ((equal (mod x 3) 0) x)
    ((equal (mod x 5) 0) x)))

(reduce #'+ (remove-if-not
             #'3or5p
             (loop :for i :below 1000 :collect i)))
