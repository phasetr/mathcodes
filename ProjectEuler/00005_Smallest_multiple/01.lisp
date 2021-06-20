;;; # Smallest multiple
;;; - [URL](https://projecteuler.net/problem=5)
;;; ## Problem 5
;;; 2520 is the smallest number that can be divided by each of the numbers from 1 to 10 without any remainder.
;;; What is the smallest positive number that is evenly divisible by all of the numbers from 1 to 20?
;;;
;;; 2520 は 1 から 10 の各数で割り切れる最小の数である。
;;; 1 から 20 のすべての数で割り切れる最小の正の数は何か？
;;;
;;; 最小公倍数を求めればいい
;;; 小さい方から 2 数の最小公倍数を求めていってそれを積めば終わる。
(defun mygcd (a b)
  (let ((s a) (l b) (r 0))
    (when (< b a)
     (setq s b) (setq l a))
    (setq r (mod l s))
    (if (= r 0)
        s
        (mygcd r s))))

(defun mylcm (a b)
  (/ (* a b) (gcd a b)))

(reduce #'mylcm (loop for i from 1 to 10 collecting i))
(reduce #'mylcm (loop for i from 1 to 20 collecting i))
