(* Modula-2 PIM4 - Hello World *)
MODULE HelloModula2;

FROM InOut IMPORT WriteString, WriteLn, ReadString;

VAR
  name: ARRAY [0..39] OF CHAR;

BEGIN
  WriteString('Hello from Modula-2!');
  WriteLn;
  WriteString('Niklaus Wirth, 1978');
  WriteLn;
  WriteLn;
  WriteString('Enter your name: ');
  ReadString(name);
  WriteString('Greetings, ');
  WriteString(name);
  WriteString('!');
  WriteLn;
END HelloModula2.
