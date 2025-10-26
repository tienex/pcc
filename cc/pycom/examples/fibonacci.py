# Fibonacci sequence

def fibonacci(n):
    if n <= 1:
        return n
    else:
        return fibonacci(n - 1) + fibonacci(n - 2)

def main():
    i = 0
    while i < 10:
        result = fibonacci(i)
        print(result)
        i = i + 1
    return 0

main()
