comment Mathematical functions in ALGOL 60;

begin
  real x, y;

  x := 2.0;

  comment Test standard math functions;
  y := sqrt(x);
  write("sqrt(", x, ") = ", y);

  y := sin(x);
  write("sin(", x, ") = ", y);

  y := cos(x);
  write("cos(", x, ") = ", y);

  y := ln(x);
  write("ln(", x, ") = ", y);

  y := exp(x);
  write("exp(", x, ") = ", y);

  y := abs(-x);
  write("abs(-", x, ") = ", y)
end
