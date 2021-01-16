#ifndef AUT_H
#define AUT_H

#include <stddef.h>
#include <stdlib.h>
#include <stdio.h>

#include "action.h"
#include "value.h"

//-----------------------------------------------------------------------//
// Runtime Library: Types and Structures
//-----------------------------------------------------------------------//

typedef struct _ActionStatePair {
    Action* pAction;
    int state;
} ActionStatePair;

typedef struct _Choice {
    int tag;
    int len;
    ActionStatePair * transitions ;
} Choice ;

typedef struct _Aut {
    int initial;
    Choice * table;
    int accepting;
} Aut ;


#define EMPTYCHOICE 0
#define UNICHOICE 1
#define SEQCHOICE 2
#define PARCHOICE 3

extern char * actionToString(Action * act);

#endif /* AUT_H */
