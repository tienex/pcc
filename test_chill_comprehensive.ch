/* Comprehensive CHILL Test Program
 * Tests various CHILL language features and runtime library functions
 */

MODULE test_program;

SPEC
  /* Test basic declarations */
  DCL counter INT := 0;
  DCL message CHARS(50) := "CHILL Compiler Test";
  DCL pi REAL := 3.14159;
  DCL flag BOOL := TRUE;

  /* Test arrays */
  DCL numbers ARRAY(0:9) OF INT;

  /* Test structures */
  SYNMODE point = STRUCT (
    x INT,
    y INT
  );

  DCL origin point;

  /* Procedures */
  PROC print_header() VOID;
  PROC test_arithmetic() VOID;
  PROC test_strings() VOID;
  PROC test_control_flow() VOID;
  PROC main_program() VOID;

END;

BODY

/* Print header procedure */
print_header: PROC() VOID;
  /* Print program title */
  /* WRITE("===================================");
     WRITE(message);
     WRITE("==================================="); */
END print_header;

/* Test arithmetic operations */
test_arithmetic: PROC() VOID;
  DCL a INT := 10;
  DCL b INT := 5;
  DCL result INT;

  /* Basic arithmetic */
  result := a + b;
  result := a - b;
  result := a * b;
  result := a / b;
  result := a MOD b;

  /* Comparison */
  IF a > b THEN
    result := 1;
  ELSE
    result := 0;
  FI;

END test_arithmetic;

/* Test string operations */
test_strings: PROC() VOID;
  DCL str1 CHARS(20) := "Hello";
  DCL str2 CHARS(20) := "World";
  DCL combined CHARS(50);

  /* String assignment and manipulation would go here */
  /* In a full implementation, we'd use runtime library functions */

END test_strings;

/* Test control flow structures */
test_control_flow: PROC() VOID;
  DCL i INT;
  DCL sum INT := 0;

  /* DO loop */
  DO i := 1 TO 10;
    sum := sum + i;
  OD;

  /* WHILE loop */
  i := 0;
  WHILE i < 5 DO
    i := i + 1;
  OD;

  /* CASE statement */
  CASE i OF
    (1): sum := sum + 1;
    (2): sum := sum + 2;
    (3): sum := sum + 3;
    ELSE: sum := 0;
  ESAC;

END test_control_flow;

/* Main program */
main_program: PROC() VOID;
  /* Initialize data */
  counter := 0;
  origin.x := 0;
  origin.y := 0;

  /* Run tests */
  print_header();
  test_arithmetic();
  test_strings();
  test_control_flow();

  /* Print completion message */
  /* WRITE("Tests completed successfully!"); */

END main_program;

/* Module initialization */
/* Call main program */
main_program();

END;
