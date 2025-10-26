(* Oberon-2 - Hello World *)
MODULE Hello;

IMPORT Out;

TYPE
  Greeter* = POINTER TO GreeterDesc;
  GreeterDesc* = RECORD
    name*: ARRAY 64 OF CHAR;
  END;

PROCEDURE (g: Greeter) Greet*;
BEGIN
  Out.String("Hello, ");
  Out.String(g.name);
  Out.String("!");
  Out.Ln;
  Out.String("Welcome to Oberon-2");
  Out.Ln;
END Greet;

VAR
  g: Greeter;

BEGIN
  NEW(g);
  g.name := "World";
  g.Greet();
END Hello.
