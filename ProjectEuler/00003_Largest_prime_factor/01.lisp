;;; # Largest prime factor
;;; - [URL](https://projecteuler.net/problem=3)
;;; ## Problem 3
;;; The prime factors of 13195 are 5, 7, 13 and 29.
;;; What is the largest prime factor of the number 600851475143?
;;;
;;; 13195 の素因数は 5, 7, 13, 29 である。
;;; 600851475143 の最大の素因数は何か？
(defun solve1 (orig-n n divnum maxnum)
  (cond ((< (* divnum divnum) orig-n)
         (if (= (mod n divnum) 0)
             (solve1 orig-n (/ n divnum) divnum divnum)
             (solve1 orig-n n (incf divnum) maxnum)))
        (t (if (= n 1)
               maxnum
               orig-n))))

(let ((target 600851475143))
  (solve1 target target 2 1))

(defun solve2 (orig-n n divnum maxnum)
  (if (< (* divnum divnum) orig-n)
      (if (= (mod n divnum) 0)
          (solve2 orig-n (/ n divnum) divnum divnum)
          (solve2 orig-n n (incf divnum) maxnum))
      (if (= n 1)
          maxnum
          orig-n)))

(let ((target 600851475143))
  (solve2 target target 2 1))
