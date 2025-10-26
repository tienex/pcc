# PXC Dialect Compatibility

PXC (Portable Xbase++ Compiler) supports compilation of source code from all major Xbase language dialects, from the earliest dBASE II (1981) to modern Xbase++ (1997-present).

## Supported Dialects

### dBASE Family

#### dBASE II (1981)
- **Command**: `-D dbase2`
- **Features**: Basic procedural programming, PRIVATE/PUBLIC variables
- **Keywords**: FUNCTION, PROCEDURE, PRIVATE, PUBLIC

#### dBASE III (1984)
- **Command**: `-D dbase3`
- **Features**: Enhanced dBASE II with User-Defined Functions (UDFs)
- **Keywords**: All dBASE II + FIELD keyword
- **Functions**: RECNO, EOF, BOF, FOUND, DELETED, etc.

#### dBASE IV (1988)
- **Command**: `-D dbase4`
- **Features**: Major update with SQL support and LOCAL variables
- **Keywords**: All dBASE III + LOCAL
- **Functions**: Extended database functions, query support

### Clipper (1985-1997)

#### CA-Clipper 5.x
- **Command**: `-D clipper`
- **Features**: Industry standard compiler with codeblocks and advanced arrays
- **Keywords**: LOCAL, STATIC, SEQUENCE, RECOVER
- **Unique Features**:
  - Codeblocks: `{|x| x * 2}`
  - BEGIN SEQUENCE ... RECOVER ... END
  - Advanced array functions: AEVAL, ASCAN, ACOPY, ACLONE
  - Screen functions: SAVESCREEN, RESTSCREEN, DEVOUT
  - Memo functions: MLCOUNT, MLPOS, MLINE

**Example**:
```xbase
FUNCTION Main()
   LOCAL aData := {1, 2, 3}
   LOCAL nSum := 0

   AEVAL(aData, {|x| nSum += x})

   RETURN nSum
```

### FoxPro Family

#### FoxPro 2.x (1991)
- **Command**: `-D foxpro`
- **Features**: Enhanced dBASE with improved performance
- **Keywords**: LOCAL, PRIVATE, PUBLIC, FIELD
- **Functions**: Extended string and date functions

#### Visual FoxPro (1995-2007)
- **Command**: `-D vfp`
- **Features**: GUI, OOP, SQL, event-driven programming
- **Keywords**: CLASS, METHOD, TRY, CATCH, FOREACH, WITH OBJECT
- **Unique Features**:
  - Object-Oriented Programming
  - Event binding: BINDEVENT, UNBINDEVENTS
  - SQL: SQLEXEC, SQLCONNECT
  - Advanced objects: CREATEOBJECT, ADDOBJECT, DODEFAULT
  - Property/method introspection: PEMSTATUS, AMEMBERS

**Example**:
```xbase
DEFINE CLASS Customer AS CUSTOM
   cName = ""
   nAge = 0

   PROCEDURE SetName(cNewName)
      THIS.cName = cNewName
   ENDPROC
ENDDEFINE

FUNCTION Main()
   LOCAL oCustomer
   oCustomer = CREATEOBJECT("Customer")
   oCustomer.SetName("John")
   RETURN
```

### Harbour (1999-present)

#### Harbour
- **Command**: `-D harbour`
- **Features**: Open-source Clipper-compatible compiler with extensive extensions
- **Keywords**: All Clipper keywords + TRY/CATCH, FOREACH, SWITCH
- **Unique Features**:
  - Hash tables: HB_HSET, HB_HGET, HB_HKEYS, HB_HVALUES
  - JSON: HB_JSONENCODE, HB_JSONDECODE
  - Cryptography: HB_MD5, HB_SHA256, HB_AES, HB_BLOWFISH
  - Base64: HB_BASE64ENCODE, HB_BASE64DECODE
  - Regular expressions: HB_REGEX
  - Threading: HB_THREADSTART, HB_THREADJOIN
  - Mutexes: HB_MUTEXCREATE, HB_MUTEXLOCK
  - Serialization: HB_SERIALIZE, HB_DESERIALIZE

**Example**:
```xbase
FUNCTION Main()
   LOCAL hData := {=>}
   LOCAL cJson

   HB_HSET(hData, "name", "Harbour")
   HB_HSET(hData, "version", "3.4")

   cJson := HB_JSONENCODE(hData)
   ? "JSON:", cJson

   RETURN 0
```

### xHarbour (2001-present)

#### xHarbour
- **Command**: `-D xharbour`
- **Features**: Extended Harbour with additional features
- **Keywords**: All Harbour keywords
- **Functions**: All Harbour functions + additional extensions
- **Differences from Harbour**: Enhanced OOP, additional utility functions

### Xbase++ (1997-present)

#### Xbase++ [DEFAULT]
- **Command**: `-D xbasepp` (or no -D flag)
- **Features**: Modern commercial Xbase with full OOP, threading, namespaces
- **Keywords**: All previous keywords + NAMESPACE
- **Unique Features**:
  - Full object-oriented programming with multiple inheritance
  - Namespaces for code organization
  - Advanced threading with THREADOBJECT
  - Exception handling with THROW
  - Class introspection: CLASSOBJECT, CLASSNAME, CLASSMETHOD
  - Code evaluation: OBJECTFROMCODE, CODEBLOCKFROMCODE
  - Type checking: ISDERIVEDCLASS

**Example**:
```xbase
CLASS Customer
   EXPORTED:
   VAR cName
   VAR nAge

   METHOD Init(cName, nAge)
   METHOD Display()
ENDCLASS

METHOD Customer:Init(cName, nAge)
   ::cName := cName
   ::nAge := nAge
   RETURN Self

METHOD Customer:Display()
   ? "Customer:", ::cName, "Age:", ::nAge
   RETURN NIL

FUNCTION Main()
   LOCAL oCustomer := Customer():New("John", 42)
   oCustomer:Display()
   RETURN 0
```

## Auto-Detection

#### Auto Mode
- **Command**: `-D auto`
- **Features**: Automatically detects dialect from source code
- **Detection Logic**:
  - NAMESPACE → Xbase++
  - CLASS + TRY + INLINE → xHarbour
  - WITH OBJECT + TRY → Visual FoxPro
  - CLASS + FOREACH → Harbour
  - CLASS → Harbour (default for OOP)
  - LOCAL + codeblocks → Clipper
  - LOCAL → dBASE IV
  - Default → dBASE III

## Feature Comparison

| Feature | dB2 | dB3 | dB4 | Clipper | FoxPro | VFP | Harbour | xHarbour | Xbase++ |
|---------|-----|-----|-----|---------|--------|-----|---------|----------|---------|
| PROCEDURE | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ |
| LOCAL | - | - | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ |
| STATIC | - | - | - | ✓ | - | - | ✓ | ✓ | ✓ |
| OOP | - | - | - | - | - | ✓ | ✓ | ✓ | ✓ |
| Codeblocks | - | - | - | ✓ | - | - | ✓ | ✓ | ✓ |
| TRY/CATCH | - | - | - | - | - | ✓ | ✓ | ✓ | ✓ |
| FOREACH | - | - | - | - | - | ✓ | ✓ | ✓ | ✓ |
| Hash Tables | - | - | - | - | - | - | ✓ | ✓ | ✓ |
| Threading | - | - | - | - | - | - | ✓ | ✓ | ✓ |
| Namespaces | - | - | - | - | - | - | - | - | ✓ |

## Built-in Functions by Dialect

### Common Functions (All Dialects)
String: LEN, SUBSTR, LEFT, RIGHT, UPPER, LOWER, TRIM, LTRIM, RTRIM, SPACE, AT, RAT, STRTRAN, STUFF, CHR, ASC

Numeric: ABS, INT, ROUND, SQRT, EXP, LOG, SIN, COS, TAN, MIN, MAX, MOD, RAND

Type: STR, VAL, CTOD, DTOC, DTOS, STOD, TRANSFORM

Date: DATE, YEAR, MONTH, DAY, DOW, CDOW, CMONTH

### Clipper-Specific
Array: AEVAL, ASCAN, ACOPY, ACLONE, AINS, ADEL, ASIZE, AFILL, AADD, ASORT, ATAIL

Screen: SAVESCREEN, RESTSCREEN, SETCOLOR, SETCURSOR, DEVPOS, DEVOUT, SCROLL

String: PADR, PADL, PADC, STRZERO, REPLICATE

Memo: MLCOUNT, MLPOS, MLINE, HARDCR

### Visual FoxPro-Specific
OOP: CREATEOBJECT, ADDOBJECT, REMOVEOBJECT, DODEFAULT, BINDEVENT, PEMSTATUS, AMEMBERS

SQL: SQLEXEC, SQLCONNECT, SQLDISCONNECT, CURSORGETPROP, CURSORSETPROP

Utility: GETWORDCOUNT, GETWORDNUM, STREXTRACT, TEXTMERGE, EVALUATE, EXECSCRIPT, SYS

### Harbour/xHarbour-Specific
Hash: HB_HSET, HB_HGET, HB_HKEYS, HB_HVALUES, HB_HHASKEY, HB_HDEL, HB_HCLONE, HB_HMERGE

JSON: HB_JSONENCODE, HB_JSONDECODE

Crypto: HB_MD5, HB_SHA1, HB_SHA256, HB_BLOWFISH, HB_AES

Base64: HB_BASE64ENCODE, HB_BASE64DECODE

Thread: HB_THREADSTART, HB_THREADJOIN, HB_THREADQUIT

Mutex: HB_MUTEXCREATE, HB_MUTEXLOCK, HB_MUTEXUNLOCK

Serialization: HB_SERIALIZE, HB_DESERIALIZE, HB_VALTOEXP

Regex: HB_REGEX, HB_ATOKENS

### Xbase++-Specific
Class: CLASSOBJECT, CLASSNAME, CLASSMETHOD, ISDERIVEDCLASS

Object: OBJECTFROMCODE, CODEBLOCKFROMCODE

Application: APPOBJECT, APPNAME, SETAPPFOCUS

Thread: THREADOBJECT, THREADSLEEP, THREADWAIT

Exception: THROW, BEGINSEQUENCE, ENDSEQUENCE

## Usage Examples

### Compile for specific dialect:
```bash
# Compile for Clipper
pxccom -D clipper myapp.prg -o myapp.c

# Compile for Visual FoxPro
pxccom -D vfp myapp.prg -o myapp.c

# Compile for Harbour
pxccom -D harbour myapp.prg -o myapp.c

# Auto-detect dialect
pxccom -D auto myapp.prg -o myapp.c

# Default (Xbase++)
pxccom myapp.prg -o myapp.c
```

### View help:
```bash
pxccom --help
```

## Compatibility Notes

1. **Default Dialect**: If no `-D` option is specified, PXC defaults to Xbase++ mode (most permissive)

2. **Strict Mode**: Currently all features are available in all dialects. Future versions may add a strict mode that enforces dialect-specific restrictions.

3. **Feature Detection**: When compiling with a specific dialect, PXC allows using features from that dialect and all earlier dialects.

4. **Runtime Library**: All functions compile to calls to the Xbase Runtime Library (libxbrt), which provides implementations for all dialect-specific functions.

5. **Code Generation**: PXC generates portable C code that can be compiled with any C compiler (GCC, Clang, MSVC, PCC).

## Migration Guide

### From dBASE to Clipper:
- Replace PRIVATE with LOCAL where possible
- Add codeblocks for array operations
- Use BEGIN SEQUENCE for error handling

### From Clipper to Harbour:
- Add HB_ prefix functions for new features
- Consider using hash tables instead of arrays for key-value data
- Use TRY/CATCH instead of BEGIN SEQUENCE

### From Visual FoxPro to Harbour:
- Replace VFP SQL with Harbour's RDDADS or PostgreSQL support
- Convert VFP objects to Harbour classes
- Replace BINDEVENT with manual event handling

### From Harbour to Xbase++:
- Organize code with namespaces
- Use Xbase++ class syntax
- Leverage thread objects for concurrent programming

## Conclusion

PXC provides comprehensive support for all major Xbase dialects, allowing you to:
- Compile legacy dBASE code from the 1980s
- Port Clipper applications to modern platforms
- Migrate Visual FoxPro applications
- Develop new applications with modern Xbase++ features

The dialect system ensures backward compatibility while enabling modern features for new development.
