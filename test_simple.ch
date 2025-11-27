/* Minimal CHILL test program */
MODULE test;

SPEC
  DCL x INT;
  DCL y INT;
END;

BODY

/* Test procedure */
test_proc: PROC() VOID;
  x := 10;
  y := 20;
END test_proc;

END;
