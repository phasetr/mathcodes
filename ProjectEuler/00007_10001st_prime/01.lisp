;;; # 10001st prime
;;; - [URL](https://projecteuler.net/problem=7)
;;; ## Problem 7
;;; By listing the first six prime numbers: 2, 3, 5, 7, 11, and 13, we can see that the 6th prime is 13.
;;; What is the 10 001st prime number?
;;;
;;; はじめの 6 個の素数をリストすると 6 番目の素数が 13 であることがわかる。
;;; 10001 番の素数は何か？

(defun primep (n)
  "Return T if N is a prime number, NIL otherwise."
  (and (integerp n)
       (> n 1)
       (loop for i from 2 to (isqrt n)
             never (zerop (rem n i)))))
(mapcar #'primep (loop for i from 1 to 10 collecting i))
(primep 2)

(defun primes (n next-i origps)
  (cond
    ((= n (length origps)) origps)
    ((primep next-i)
     (setq origps (cons next-i origps))
     (primes  n (incf next-i) origps))
    (t (primes n (incf next-i) origps))))

(defun solve1 (n)
  (car (primes n 1 '())))
(solve1 10001)
