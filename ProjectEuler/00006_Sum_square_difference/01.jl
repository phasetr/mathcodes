# # Sum square difference
# - [URL](https://projecteuler.net/problem=6)
# ## Problem 6
# The sum of the squares of the first ten natural numbers is,
# $1^2 + 2^2 + \cdots + 10^2 = 385$.
# The square of the sum of the first ten natural numbers is,
# $(1 + 2 + \cdots + 10)^2 = 55^2 = 3025$.
# Hence the difference between the sum of the squares of the first ten natural numbers
# and the square of the sum is $3025 - 385 = 2640$.
# Find the difference between the sum of the squares of the first one hundred natural numbers
# and the square of the sum.
#
# 自然数の最初の 10 個の 2 乗の総和は $385$ である。
# 自然数の最初の 10 個の総和の 2 乗は $3025$ である。
# したがってこの 2 数の差は $2640$ である。
# 自然数の最初の 100 個について同じように差を求めよ。

solve(a) = sum(a)^2 - sum(map(x -> x^2, a))
solve(Array(1:10))
solve(Array(1:100))
