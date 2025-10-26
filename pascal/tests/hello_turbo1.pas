{ Turbo Pascal 1.0 (CP/M) - Hello World }
{ Very simple, early version from 1983 }
program HelloTurbo1;

var
  name: string[40];

begin
  writeln('Hello from Turbo Pascal 1.0!');
  writeln('The legendary CP/M compiler');
  writeln;
  write('What is your name? ');
  readln(name);
  writeln('Greetings, ', name, '!');
end.
