*
* dBASE III/IV compatibility test
* Classic dBASE syntax
*

FUNCTION Main
   PRIVATE cName, nAge, dBirth

   * Variable initialization
   cName = "John Doe"
   nAge = 42
   dBirth = CTOD("01/15/1982")

   * Output
   ? "dBASE Test"
   ? "Name:", cName
   ? "Age:", nAge
   ? "Birth:", DTOC(dBirth)
   ? "Year:", YEAR(dBirth)

   RETURN
