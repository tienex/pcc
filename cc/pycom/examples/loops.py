# Loop examples

def count_up():
    i = 0
    while i < 10:
        print(i)
        i = i + 1
    return 0

def sum_numbers(n):
    total = 0
    i = 1
    while i <= n:
        total = total + i
        i = i + 1
    return total

def main():
    count_up()
    result = sum_numbers(100)
    print(result)
    return 0

main()
