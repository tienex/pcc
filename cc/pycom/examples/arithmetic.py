# Arithmetic operations

def test_arithmetic():
    a = 10
    b = 20

    # Basic operations
    sum_val = a + b
    diff = b - a
    prod = a * b
    quot = b / a
    mod = b % a

    # Bitwise operations
    and_val = a & b
    or_val = a | b
    xor_val = a ^ b
    lshift = a << 2
    rshift = b >> 1

    return sum_val

def main():
    result = test_arithmetic()
    print(result)
    return 0

main()
