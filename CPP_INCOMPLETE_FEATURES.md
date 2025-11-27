# PCC C++ Compiler - Incomplete Features Analysis

**Date:** 2025-10-26
**Analysis:** Identify incomplete code and features to implement

---

## Infrastructure Already Present

### ✅ Complete Infrastructure
1. **Namespace handling** - `dclns()`, namespace symbol table management
2. **Name mangling** - Itanium ABI-style decoration
3. **new/delete operators** - `cxx_new()`, `cxx_delete()`
4. **C++ casts** - Tokens recognized (const_cast, static_cast, etc.)
5. **Linkage specification** - `extern "C"` support

### ⚠️ Partially Implemented

#### Classes/Structs (Foundation Exists)
**Location:** `cc/cxxcom/cxxcode.c`, `cgram.y`

**What's there:**
- `cxxdclstr()` - Declare a class (lines 548-564)
- `cxxstrvar()` - Create `__%THIS` pointer for member functions (lines 526-538)
- `cxxrstruct()` - Reference struct via `::` operator (lines 738-756)
- `cxxmatchftn()` - Match member function by signature (lines 766-788)
- `cxxaddhidden()` - Add hidden `this` parameter (lines 794-809)
- `cxxstructref()` - Handle `.` and `->` for members (lines 812-844)
- `cxxmember()` - Member handling (prototype in cxxdefs.h)

**What's incomplete:**
```c
// Line 557-560 in cxxcode.c - COMMENTED OUT!
//	else
//		uerror("class/namespace redefined");
//	INSSYM(sp);
//	nscur = sp;
```

**Access Control (Started but disabled):**
```c
// Line 704 in cgram.y - COMMENTED OUT!
|  C_NAME ':' { /* cxxaccess($1); */ }
```
The grammar recognizes `public:`, `private:`, `protected:` but does nothing with them.

#### Member Functions
**What's there:**
- Hidden `this` pointer infrastructure (`__%THIS`)
- Member function name decoration
- Function matching by signature

**What's missing:**
- Constructor/destructor special handling
- Const member functions
- Static member functions
- Member initialization lists

#### Constructors/Destructors
**Status:** NOT IMPLEMENTED
- No special parsing rules
- No automatic invocation
- No initialization list handling
- No destructor chaining for RAII

---

## XXX Comments (Code TODOs)

### High Priority Fixes

**1. Type encoding in name mangling** (`cxxcode.c:294`)
```c
/* XXX - cannot emit const/volatile */
nmch(typch(t));
```
**Impact:** Name mangling doesn't handle cv-qualifiers

**2. Attribute checking** (`cxxcode.c:66`)
```c
sp->sap = attr_add(sp->sap, ap); /* XXX check attributes */
```
**Impact:** Attributes not validated

**3. Function definition** (`cgram.y:718`)
```c
n->n_qual |= 1; /* definition place XXX used by attributes */
```
**Impact:** Unclear usage of n_qual field

**4. Array dimension checking** (`pftn.c:819`)
```c
/* XXX - check dimensions at usage time */
```
**Impact:** Array bounds not validated

### Medium Priority

**5. GCC pragma support** (`gcc_compat.c`)
```c
werror("gcc pragma unsupported");
```
**Impact:** GCC compatibility reduced

**6. Inline function limitations** (`inline.c`)
```c
return NIL; /* XXX cannot handle hidden ebx arg */
```
**Impact:** Some inline functions won't work

---

## Missing C++98 Features (Priority Order)

### TIER 1: Foundation (Build on existing infrastructure)

#### 1.1 Complete Class Support ⭐ EASIEST
**Effort:** 2-4 hours
**Files:** `cxxcode.c`, `cgram.y`

**Tasks:**
- [ ] Un-comment class scope management (lines 557-560)
- [ ] Implement `cxxaccess()` function for public/private/protected
- [ ] Add access checking to member lookup
- [ ] Test: Simple class with public/private members

**Example target:**
```cpp
class Point {
private:
    int x;
public:
    int y;
    void setX(int val) { x = val; }
};
```

#### 1.2 Member Functions with `this`
**Effort:** 3-5 hours
**Files:** `cxxcode.c`, `cgram.y`, `pftn.c`

**Tasks:**
- [ ] Ensure `__%THIS` is properly added to member functions
- [ ] Make `this` keyword work in member function bodies
- [ ] Handle implicit object parameter in calls
- [ ] Test: Method calls on objects

#### 1.3 Basic Constructors
**Effort:** 5-8 hours
**Files:** `cxxcode.c`, `cgram.y`, `init.c`

**Tasks:**
- [ ] Recognize constructor (function name == class name)
- [ ] Generate default constructor if none provided
- [ ] Call constructor on object creation
- [ ] Test: Constructor with initialization

**Example target:**
```cpp
class Foo {
public:
    int value;
    Foo() { value = 0; }
    Foo(int v) { value = v; }
};

Foo f1;        // calls Foo()
Foo f2(42);    // calls Foo(int)
```

#### 1.4 Basic Destructors
**Effort:** 4-6 hours
**Files:** `cxxcode.c`, `cgram.y`

**Tasks:**
- [ ] Recognize destructor (`~ClassName`)
- [ ] Generate default destructor
- [ ] Call destructor at scope exit
- [ ] Test: RAII pattern

### TIER 2: Essential Features

#### 2.1 References (`T&`)
**Effort:** 8-12 hours
**Files:** Multiple - type system changes

**Tasks:**
- [ ] Add reference type to type system (TREF?)
- [ ] Reference initialization semantics
- [ ] Prevent reference rebinding
- [ ] Pass-by-reference

#### 2.2 Function Overloading
**Effort:** 6-10 hours (name mangling mostly done)
**Files:** `cxxcode.c`, `pftn.c`

**Tasks:**
- [ ] Overload resolution algorithm
- [ ] Argument type matching and conversions
- [ ] Best match selection
- [ ] Already have name mangling!

#### 2.3 Operator Overloading
**Effort:** 10-15 hours
**Files:** `cgram.y`, `cxxcode.c`, `trees.c`

**Tasks:**
- [ ] Parse operator definitions
- [ ] Map operators to function calls
- [ ] Handle member vs. free operators
- [ ] Special operators (=, [], (), ->)

### TIER 3: Advanced Features

#### 3.1 Inheritance (Single)
**Effort:** 15-20 hours

**Tasks:**
- [ ] Base class specifiers in grammar
- [ ] Member lookup in base classes
- [ ] Constructor chaining
- [ ] Type conversion (derived to base)

#### 3.2 Virtual Functions
**Effort:** 20-30 hours

**Tasks:**
- [ ] vtable generation
- [ ] vptr in objects
- [ ] Virtual function dispatch
- [ ] Pure virtual (abstract classes)

#### 3.3 Templates
**Effort:** 40-60+ hours (VERY COMPLEX)

**Tasks:**
- [ ] Template parsing
- [ ] Template AST representation
- [ ] Instantiation engine
- [ ] Specialization
- [ ] SFINAE

#### 3.4 Exception Handling
**Effort:** 30-50 hours (VERY COMPLEX)

**Tasks:**
- [ ] try/catch/throw parsing
- [ ] Exception tables
- [ ] Stack unwinding
- [ ] Destructor calls during unwinding
- [ ] RTTI for exception matching

---

## Immediate Next Steps (Quick Wins)

### Step 1: Enable Basic Class Scope (30 minutes)
**File:** `cc/cxxcom/cxxcode.c:557-560`

```c
// BEFORE:
if (sp == 0)
    sp = getsymtab(n, STAGNAME);
//	else
//		uerror("class/namespace redefined");
//	INSSYM(sp);
//	nscur = sp;

// AFTER:
if (sp == 0) {
    sp = getsymtab(n, STAGNAME);
    INSSYM(sp);
} else {
    uerror("class/namespace redefined");
}
nscur = sp;
```

### Step 2: Implement Access Control (1-2 hours)
**File:** `cc/cxxcom/cxxcode.c` (new function)

```c
void cxxaccess(char *name)
{
    // Called when: public: private: protected: appears
    // Set current access level for subsequent members
}
```

### Step 3: Create Test Suite
**File:** `tests/cxx_basic.cpp`

```cpp
// Test 1: Namespace
namespace Test {
    int value = 42;
}

// Test 2: Simple class
class Point {
public:
    int x;
    int y;
};

// Test 3: Class with private
class Counter {
private:
    int count;
public:
    void set(int c) { count = c; }
};
```

---

## Recommended Implementation Order

### Phase 1: Foundation (Week 1)
1. ✅ Fix build issues (DONE)
2. Enable class scope management
3. Implement access control (public/private/protected)
4. Fix member access checking
5. Test: Classes with members

### Phase 2: Methods (Week 2)
6. Complete `this` pointer handling
7. Member function calls
8. Test: Methods modifying members

### Phase 3: Lifecycle (Week 3)
9. Basic constructors
10. Basic destructors
11. Test: RAII pattern

### Phase 4: References & Overloading (Week 4)
12. Reference types
13. Function overloading
14. Test: Overloaded functions

### Phase 5: Operators (Week 5-6)
15. Operator overloading
16. Test: Operator+ for custom types

### Beyond Phase 5
- Inheritance (single)
- Virtual functions
- Templates (MAJOR effort)
- Exception handling (MAJOR effort)

---

## Files to Modify

**Primary:**
- `cc/cxxcom/cxxcode.c` - Core C++ semantics
- `cc/cxxcom/cgram.y` - Grammar additions
- `cc/cxxcom/cxxdefs.h` - API definitions

**Secondary:**
- `cc/cxxcom/pftn.c` - Function handling
- `cc/cxxcom/init.c` - Constructor calls
- `cc/cxxcom/trees.c` - Member access
- `cc/cxxcom/scan.l` - New keywords if needed

**Testing:**
- Create `tests/` directory
- Add incremental C++ test cases

---

## Conclusion

The PCC C++ compiler has **substantial infrastructure already in place** but is incomplete. The easiest path to basic functionality:

**Quick wins (< 1 week):**
- Enable class scopes
- Add access control
- Complete member functions with `this`

**Medium effort (2-4 weeks):**
- Constructors/destructors
- References
- Function overloading

**Long-term (months):**
- Inheritance
- Virtual functions
- Templates
- Exception handling

**Recommended:** Start with Phase 1 to get classes working, then evaluate whether to continue based on project goals.

