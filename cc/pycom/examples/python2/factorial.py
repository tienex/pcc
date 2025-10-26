# Python 2 factorial using print statement

def factorial(n):
    if n <= 1:
        return 1
    else:
        return n * factorial(n - 1)

def main():
    print "Factorial of 5:"
    result = factorial(5)
    print result
    return 0

main()
