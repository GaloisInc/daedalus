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


#define MAKE_ACT_READCHAR(c) {.tag = ACT_ReadChar, .chr = c}
#define MAKE_ACT_STORE_SEM(v) {.tag = ACT_STORE_SEM, .kv = v}
#define MAKE_ACT_PUSH(v) {.tag = ACT_PUSH, .state = v}
#define MAKE_ACT_POP() {.tag = ACT_Pop}
#define MAKE_ACT_END {.tag = ACT_END}

extern char * action_to_string(Action * act);

#endif /* AUT_H */
