/* Simple CHILL test program */
MODULE hello;

SPEC
  DCL message CHARS(13) := "Hello, World!";
  GRANT message;

END;
