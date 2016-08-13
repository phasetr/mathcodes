# coding: utf-8
"""
The prime factors of 13195 are 5, 7, 13 and 29.

What is the largest prime factor of the number 600851475143?
"""
import math

# 考える数
now_number = 600851475143
now_number_sqrt = math.ceil(math.sqrt(now_number))

if __name__ == "__main__":
    # 素数かどうかは抜きにしてとにかく割れる最大数を出す
    max_number = 1
    for i in range(2, now_number_sqrt):
        if now_number % i == 0:
            max_number = i

    max_number2 = 1
    for i in range(2, max_number):
        if max_number % i == 0:
            max_number2 = i

    print(max_number2)

