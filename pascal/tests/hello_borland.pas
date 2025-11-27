{ Borland Pascal / Turbo Pascal - Hello World }
program HelloWorld;

uses
  Crt;  // Borland/Turbo Pascal unit

var
  name: string;  // string type (Borland extension)

begin
  writeln('Hello from Borland Pascal!');
  write('Enter your name: ');
  readln(name);
  writeln('Hello, ', name, '!');

  // C++-style comment (Borland extension)
  writeln('Press any key to continue...');
end.
