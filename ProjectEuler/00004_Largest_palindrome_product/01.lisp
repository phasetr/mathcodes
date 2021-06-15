;;; # Largest palindrome product
;;; - [URL](https://projecteuler.net/problem=4)
;;; ## Problem 4
;;; A palindromic number reads the same both ways.
;;; The largest palindrome made from the product of two 2-digit numbers is 9009 = 91 × 99.
;;; Find the largest palindrome made from the product of two 3-digit numbers.
;;;
;;; 回文数は 2 通りの同じ読み方を持つ。
;;; 2 桁の数の積からなる最大の回文数は 9009 = 91 x 99 である。
;;; 3 桁の数の積からなる最大の回文数を求めよ。
;;;
;;; 3 桁の数の積からなるリスト (Sequence) を作る、よろしくない解法

(defun palindrome-p (n)
  (equal n (reverse n)))
(palindrome-p "2")
(palindrome-p "23")
(palindrome-p "22")

(parse-integer "1")
(read-from-string "1/10")
(read-from-string "1.2")
(read-from-string "#c(1 1)")
(parse-integer "11" :radix 2)
(parse-integer "11" :radix 8)
(parse-integer "11" :radix 10)
(parse-integer "11" :radix 16)

(defun solve ()
  (reduce
   #'max
   (mapcar
    #'parse-integer
    (remove-if-not
     #'palindrome-p
     (loop for x downfrom 999 to 900
           append (loop for y downfrom 999 to 900
                        collect (format nil "~a" (* x y))))))))
(solve)
