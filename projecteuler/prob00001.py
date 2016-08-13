# coding: utf-8
"""
Multiples of 3 and 5
Problem 1

If we list all the natural numbers below 10 that are multiples of 3 or 5,
we get 3, 5, 6 and 9. The sum of these multiples is 23.

Find the sum of all the multiples of 3 or 5 below 1000.
"""

# どこまで取るか
MAX_NUM = 999

# 0 以外の 3 か 5 の倍数を取り出す
def is_multiple_3_or_5(n):
    if n == 0:
        return False
    else:
        return n % 3 == 0 or n % 5 == 0

# 入力した整数までの 3 か 5 の倍数からなる数のリストを作る
def get_nums(n):
    #return list(filter(is_multiple_3_or_5, list(range(n+1))))
    return [x for x in range(n+1) if is_multiple_3_or_5(x)]

# 数のリストを取ってその全ての和を取る
def takeSum(nums):
    return sum(nums)

if __name__ == "__main__":
    print(is_multiple_3_or_5(1) == False)
    print(is_multiple_3_or_5(3) == True)
    print(is_multiple_3_or_5(5) == True)
    print(is_multiple_3_or_5(15) == True)
    print(get_nums(1) == [])
    print(get_nums(3) == [3])
    print(get_nums(5) == [3,5])
    print(get_nums(15) == [3,5,6,9,10,12,15])
    print(sum(get_nums(3)) == 3)
    print(sum(get_nums(5)) == 8)
    print(sum(get_nums(5)) == 8)
    print("\nThe following number is the answer!")
    print(sum(get_nums(MAX_NUM)))
