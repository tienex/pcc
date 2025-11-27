/*
 * Symbol Table Management
 */

#include "pycom.h"
#include <stdlib.h>
#include <string.h>

SymbolTable *symtab_new(SymbolTable *parent) {
    SymbolTable *symtab = xmalloc(sizeof(SymbolTable));
    symtab->symbols = NULL;
    symtab->parent = parent;
    symtab->current_offset = 0;
    return symtab;
}

void symtab_free(SymbolTable *symtab) {
    if (!symtab) return;

    Symbol *sym = symtab->symbols;
    while (sym) {
        Symbol *next = sym->next;
        free(sym->name);
        free(sym);
        sym = next;
    }

    free(symtab);
}

Symbol *symtab_lookup(SymbolTable *symtab, const char *name) {
    /* Search current scope */
    Symbol *sym = symtab->symbols;
    while (sym) {
        if (strcmp(sym->name, name) == 0) {
            return sym;
        }
        sym = sym->next;
    }

    /* Search parent scope */
    if (symtab->parent) {
        return symtab_lookup(symtab->parent, name);
    }

    return NULL;
}

Symbol *symtab_lookup_local(SymbolTable *symtab, const char *name) {
    Symbol *sym = symtab->symbols;
    while (sym) {
        if (strcmp(sym->name, name) == 0) {
            return sym;
        }
        sym = sym->next;
    }
    return NULL;
}

Symbol *symtab_insert(SymbolTable *symtab, const char *name, TWORD type) {
    /* Check if symbol already exists in current scope */
    Symbol *existing = symtab_lookup_local(symtab, name);
    if (existing) {
        return existing;
    }

    /* Create new symbol */
    Symbol *sym = xmalloc(sizeof(Symbol));
    sym->name = xstrdup(name);
    sym->type = type;
    sym->offset = symtab->current_offset;
    sym->is_function = 0;
    sym->is_parameter = 0;

    /* Add to symbol table */
    sym->next = symtab->symbols;
    symtab->symbols = sym;

    /* Update offset for next variable */
    symtab->current_offset += 8; /* Assume 64-bit pointers/integers */

    return sym;
}
