def virfib_sq(n):
    print(n)
    if n <= 1:
        return n
    return (virfib_sq(n-1) + virfib_sq(n-2)) ** 2
