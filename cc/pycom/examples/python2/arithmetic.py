# Python 2 arithmetic with integer division

def test_division():
    a = 10
    b = 3

    # Python 2: / does integer division
    result = a / b
    print "10 / 3 =", result

    # Floor division
    floor = a // b
    print "10 // 3 =", floor

    # Modulo
    mod = a % b
    print "10 % 3 =", mod

    return result

def main():
    print "Python 2 Division Test"
    test_division()
    return 0

main()
