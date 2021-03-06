# coding: utf-8
"""
The prime factors of 13195 are 5, 7, 13 and 29.

What is the largest prime factor of the number 600851475143?
"""
import math

# 考える数
now_number = 600851475143

# 素数かどうかの判定
def is_prime(n):
    # 2 のときを潰す
    if n == 2:
        return True

    # 偶数なら不適
    if n % 2 == 0:
        return False

    # 以下奇数のみ
    # 3 以上の奇数のリストでループを回す
    for i in [x for x in range(3, n) if x % 2 != 0]:
        if n % i == 0:
            # n 自分自身より小さい数で割り切れるなら合成数
            return False

    # 上の処理で引っかからなかったなら素数
    return True

# ある数よりも小さい素数のリスト作成
def get_primes_less_than(n):
    return [x for x in range(2, n+1) if is_prime(x)]

# ある数 n の素因数分解の結果を返す辞書
def factorize(n):
    factorization = {}

    if n == 2:
        return {2:1}
    elif n == 3:
        return {2:0, 3:1}
    elif n == 4:
        return {2:2, 3:0}

    primes = get_primes_less_than(n)
    for p in primes:
        # 初期化
        factorization[p] = 0
        tmp_num = n
        while(tmp_num % p == 0):
            factorization[p] = factorization[p] + 1
            tmp_num = tmp_num / p

    return factorization

# 素因数分解で出てきた素数のうち最大の素数を取得
def get_largest_prime(n):
    factorization = factorize(n)
    factors = []

    # value が 0 でないキーだけ取ってくる
    for k, v in factorization.items():
        if v != 0:
            factors.append(k)

    return max(factors)

if __name__ == "__main__":
    print(is_prime(2) == True)
    print(is_prime(3) == True)
    print(is_prime(4) == False)
    print(is_prime(5) == True)
    print(is_prime(7) == True)
    print(is_prime(9) == False)
    print(get_primes_less_than(2) == [2])
    print(get_primes_less_than(3) == [2,3])
    print(get_primes_less_than(4) == [2,3])
    print(get_primes_less_than(5) == [2,3,5])
    print(get_primes_less_than(15) == [2,3,5,7,11,13])
    print(factorize(2) == {2: 1})
    print(factorize(3) == {2: 0, 3: 1})
    print(factorize(4) == {2: 2, 3: 0})
    print(factorize(5) == {2: 0, 3: 0, 5: 1})
    print(factorize(6) == {2: 1, 3: 1, 5: 0})
    print(factorize(6) == {2: 1, 3: 1, 5: 0})
    print(factorize(7) == {2: 0, 3: 0, 5: 0, 7: 1})
    print(get_largest_prime(2) == 2)
    print(get_largest_prime(3) == 3)
    print(get_largest_prime(4) == 2)
    print(get_largest_prime(5) == 5)
    print(get_largest_prime(6) == 3)
    print(get_largest_prime(7) == 7)
    print("ANSWER!")
    print(get_largest_prime(now_number))
