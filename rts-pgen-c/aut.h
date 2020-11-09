#ifndef AUT_H
#define AUT_H

#include <stddef.h>
#include <stdlib.h>
#include <stdio.h>

//-----------------------------------------------------------------------//
// Runtime Library: Types and Structures
//-----------------------------------------------------------------------//

#include "value.h"

// #define ACT_READCHAR 0
// #define ACT_STORE_SEM 1
// #define ACT_PUSH 2
// #define ACT_POP 3
// #define ACT_END 1000


typedef enum _ActionType {
    ACT_EpsA,
    ACT_ReadChar,
    ACT_Match,
    ACT_Push,
    ACT_Pop,
    ACT_ReturnBind,
    ACT_EnvFresh,
    ACT_EnvStore,
    ACT_ActivateFrame,
    ACT_DeactivateReady,
    ACT_END
} ActionType ;


typedef enum _ExprType {
    E_INT,
    E_STR,
    E_VAR
} ExprType ;

typedef struct _Expr {
    ExprType tag;
    union {
        int vInt;
        char* vBytes;
        char* name;
    };
} Expr ;

typedef struct _NameList {

} NameList ;

typedef struct KeyValuePair {
    char * key;
    Value * value;
} KeyValue;

typedef struct _Action {
    ActionType tag;
    union {
        char chr;
        int state;
        KeyValue *kv;
        Expr* expr;
        char* name;
        NameList* namelist;
    };
} Action;

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
