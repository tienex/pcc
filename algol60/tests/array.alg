comment Array operations in ALGOL 60;

begin
  integer array A[1:10];
  integer i, sum;

  comment Initialize array;
  for i := 1 step 1 until 10 do
    A[i] := i * i;

  comment Sum array elements;
  sum := 0;
  for i := 1 step 1 until 10 do
    sum := sum + A[i];

  write("Sum of squares from 1 to 10: ", sum)
end
