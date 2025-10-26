

enum { NM_NEW, NM_NWA, NM_DEL, NM_DLA, NM_NORMAL, };

enum { LINK_DEF, LINK_C }; /* linkage definitions */
extern int elnk;
#define	SCLINK	00020	/* for symtab */

/* C++ access control */
enum { ACCESS_PUBLIC, ACCESS_PRIVATE, ACCESS_PROTECTED };
extern int cxxcuraccess; /* current access level in class */

/* C++ standard versions */
enum {
	CXX_STD_98,    /* C++98 */
	CXX_STD_03,    /* C++03 (minor update to 98) */
	CXX_STD_11,    /* C++11 (was C++0x) */
	CXX_STD_14,    /* C++14 */
	CXX_STD_17,    /* C++17 */
	CXX_STD_20,    /* C++20 */
	CXX_STD_23,    /* C++23 */
	CXX_STD_26     /* C++26 (future) */
};

/* C++ ABI vendors */
enum {
	CXX_ABI_ITANIUM,  /* GCC/Clang (default) */
	CXX_ABI_MSVC,     /* Microsoft Visual C++ */
	CXX_ABI_WATCOM,   /* Watcom C++ */
	CXX_ABI_BORLAND,  /* Borland C++ */
	CXX_ABI_GNU_OLD,  /* Old GNU C++ (GCC 2.x) */
	CXX_ABI_DMC,      /* Digital Mars C++ */
	CXX_ABI_ARM       /* ARM C++ (Itanium variant) */
};

extern int cxx_standard;    /* Current C++ standard version */
extern int cxx_abi;         /* Current C++ ABI */
extern int cppdebug;

/* spole is symbol at base, nscur is where we are in the stack. */
extern struct symtab *spole, *nscur;

/* insert a symbol into this something */
#define	INSSYM(sp) (sp->snext = nscur->sup, nscur->sup = sp, sp->sdown = nscur)
#define	POPSYM()   (nscur = nscur->sdown)

/* C++-specific node types */
#define		CONST_CAST	(MAXOP+35)
#define		DYN_CAST	(MAXOP+36)
#define		REINT_CAST	(MAXOP+37)
#define		STATIC_CAST	(MAXOP+38)
#define		NEWKW		(MAXOP+39)
#define		DELETE		(MAXOP+40)
#define		NMLIST		(MAXOP+41)

/* C++-specific symtab types */
#define	CLNAME	(MAXSTCL+1)	/* symtab entry is class */
#define	NSPACE	(MAXSTCL+2)	/* symtab entry is namespace */
 
char *decoratename(struct symtab *sp, int type);
NODE *cxx_new(NODE *p);
NODE *cxx_delete(NODE *p, int del);
void dclns(NODE *attr, char *n);
void cxxaccess(char *name);
int cxxisctor(char *fname, struct symtab *classsym);
int cxxisdtor(char *fname, struct symtab *classsym);
void cxxmarkctor(struct symtab *sp);
void cxxmarkdtor(struct symtab *sp);
int cxxisclass(TWORD type);
struct symtab *cxxfindctor(struct symtab *classsym);
struct symtab *cxxfinddtor(struct symtab *classsym);
NODE *cxxgencall(struct symtab *sp, struct symtab *fnsym);
void cxxabi_init(void);
struct abi_context *cxxabi_get_context(void);
char *cxxabi_mangle_function(struct symtab *sp);
struct symtab *cxxlookup(NODE *p, int declare);
void cxxsetname(struct symtab *sp);
void cxxmember(struct symtab *sp);
struct symtab *cxxstrvar(struct symtab *so);
struct symtab *cxxdclstr(char *n);
struct symtab *cxxftnfind(NODE *p, int flags);
struct symtab *cxxdeclvar(NODE *p);
void symtree(void);
NODE *cxxrstruct(int soru, NODE *attr, NODE *t, char *tag);
NODE *cxxmatchftn(NODE *, NODE *);
NODE *cxxaddhidden(NODE *, NODE *);
NODE *cxxstructref(NODE *p, int f, char *name);
