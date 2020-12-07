#ifndef ACTION_H
#define ACTION_H

#include <stdio.h>

#include "cfg.h"

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

/** Execute an action */
Cfg * execAction(Action * act, Cfg* cfg, int arrivState);

#endif