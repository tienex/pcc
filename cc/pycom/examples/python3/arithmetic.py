# Python 3 arithmetic with true division

def test_division():
    a = 10
    b = 3

    # Python 3: / does true division (would return float)
    # For now, we'll just show integer division
    result = a // b
    print("10 // 3 =", result)

    # Floor division
    floor = a // b
    print("10 // 3 =", floor)

    # Modulo
    mod = a % b
    print("10 % 3 =", mod)

    return result

def main():
    print("Python 3 Division Test")
    test_division()
    return 0

main()
