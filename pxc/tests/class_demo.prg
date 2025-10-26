/*
 * Class demonstration in Xbase++
 */

CLASS Person
   DATA name
   DATA age

   METHOD New(cName, nAge)
      ::name := cName
      ::age := nAge
      RETURN Self

   METHOD GetInfo()
      LOCAL info
      info := "Name: " + ::name + ", Age: " + STR(::age)
      RETURN info

   METHOD Birthday()
      ::age := ::age + 1
      RETURN ::age
ENDCLASS

FUNCTION Main()
   LOCAL p

   p := Person():New("John Doe", 30)
   ? p:GetInfo()

   p:Birthday()
   ? "After birthday:", p:GetInfo()

   RETURN 0
