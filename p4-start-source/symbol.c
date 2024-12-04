/* TODO: TO BE COMPLETED */
// Name: Lemurs
// Assignment: Program 4 - Uc Part 2
// Class: CSC453 Fall 2024

#include "global.h"

/* TODO: define a symbol table/array, reuse project Pr1 */


#include <string.h>
#include <stdlib.h>
#include "global.h"

#define SYMMAX 100

Symbol symtable[SYMMAX]; //create symbol table of size symmax

int lastentry = 0;

//Check symbol table to see if symbol exists
Symbol *lookup(const char *s)
{
    /* TODO: TO BE COMPLETED */
    Symbol *result = NULL;  // Initialize result

    for (int p = 0; p < lastentry; p++) {
        if (strcmp(symtable[p].lexptr, s) == 0) {
            result = &symtable[p];  // Set result to symbol address
            break; 
        }
    }

    return result;  

}

Symbol *insert(const char *s, int token)
{
        /* TODO: TO BE COMPLETED */
    Symbol *result = lookup(s);  //see if symbol exists

    if (result == NULL && lastentry < SYMMAX - 1) {
        // Insert new symbol 
        result = &symtable[lastentry];
        result->lexptr = (char *)malloc(strlen(s) + 1);

        
        strcpy(result->lexptr, s);
        result->token = token;
        result->localvar = 0; 

        lastentry++; 
        
    } else if (result == NULL) {
        error("symbol table full");
    }

    return result;

}
