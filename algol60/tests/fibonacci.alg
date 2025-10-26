comment Fibonacci sequence in ALGOL 60;

begin
  integer n, i, a, b, temp;

  n := 10;
  a := 0;
  b := 1;

  write("Fibonacci sequence:");
  write(a);
  write(b);

  for i := 2 step 1 until n do
  begin
    temp := a + b;
    a := b;
    b := temp;
    write(temp)
  end
end
