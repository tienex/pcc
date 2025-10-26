comment Factorial computation in ALGOL 60;

begin
  integer n, result;

  comment Factorial procedure;
  integer procedure factorial(n);
    value n;
    integer n;
  begin
    if n <= 1 then
      factorial := 1
    else
      factorial := n * factorial(n - 1)
  end;

  n := 5;
  result := factorial(n);
  write("Factorial of ", n, " is ", result)
end
