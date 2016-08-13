# coding: utf-8
"""
The prime factors of 13195 are 5, 7, 13 and 29.

What is the largest prime factor of the number 600851475143?
"""
import math

# 考える数
now_number = 600851475143

# ルート以下の素数を探せばいい
now_number_sqrt = math.ceil(math.sqrt(now_number))

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

if __name__ == "__main__":
    # 素数かどうかは抜きにしてとにかく割れる最大数を出す
    max_number = 1
    for i in range(2, now_number_sqrt):
        if now_number % i == 0:
            max_number = i

    max_number2 = 1
    for i in range(2, max_number + 1):
        if max_number % i == 0:
            print(i)
            max_number2 = i

    print(max_number2)
