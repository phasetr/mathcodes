# coding: utf-8
"""
Each new term in the Fibonacci sequence is generated by adding the previous two terms.
By starting with 1 and 2, the first 10 terms will be:

1, 2, 3, 5, 8, 13, 21, 34, 55, 89, ...

By considering the terms in the Fibonacci sequence whose values do not exceed four million, find the sum of the even-valued terms.
"""
# 最大数
max_num = 4000000

# 偶数取り出し
def is_even(n):
    return n % 2 == 0

# Fibonacci
def fib(n):
    a = 1
    b = 2
    for _ in range(n):
        a, b = b, a + b
    return a

# Fibonacci が max_num になる番号を調べる
fib_index = 0
for i in range(1000000):
    if fib(i) > max_num:
        fib_index = i
        break

# 対象になる数列を作る
fibs = [fib(x) for x in range(fib_index) if is_even(fib(x))]

if __name__ == "__main__":
    print(is_even(1) == False)
    print(is_even(2) == True)
    print(fib(0) == 1)
    print(fib(1) == 2)
    print(fib(2) == 3)
    print(fib(3) == 5)
    print(fibs)
    print(sum(fibs))
