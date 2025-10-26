/*
 * Factorial calculator in Xbase++
 */

FUNCTION Main()
   LOCAL n := 5
   LOCAL result

   result := Factorial(n)
   ? "Factorial of", n, "is", result

   RETURN 0

FUNCTION Factorial(n)
   LOCAL i, result := 1

   IF n <= 0
      RETURN 1
   ENDIF

   FOR i := 1 TO n
      result := result * i
   NEXT

   RETURN result
