/*
 * Harbour compatibility test
 * Tests Harbour extensions
 */

FUNCTION Main()
   LOCAL hData := {=>}
   LOCAL aKeys, aVals
   LOCAL cJson

   * Hash table operations
   HB_HSET(hData, "name", "Harbour")
   HB_HSET(hData, "version", "3.4")
   HB_HSET(hData, "year", 1999)

   * Get hash keys and values
   aKeys := HB_HKEYS(hData)
   aVals := HB_HVALUES(hData)

   * JSON encoding
   cJson := HB_JSONENCODE(hData)

   * Output
   ? "Harbour Test"
   ? "Hash keys:", ALEN(aKeys)
   ? "JSON:", cJson
   ? "Version:", HB_VERSION()

   RETURN 0
