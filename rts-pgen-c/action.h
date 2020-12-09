#ifndef ACTION_H
#define ACTION_H

#include <stdio.h>

#include "cfg.h"

//--------------------------------------------------------------------------//
// Expression Type
//--------------------------------------------------------------------------//

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


/*

Action
   ACT_EpsA
   ACT_InputAction
   ACT_ControlAction
   ACT_SemanticAction



InputAction
    ACT_IEnd                ---> []
    ACT_IMatchBytes         ---> WithSem VExpr
    ACT_ReadChar            ---> char

ControlAction
    ACT_Push                ---> Name, [VExpr], State
    ACT_Pop                 ---> []
    ACT_ActivateFrame       ---> NameList
    ACT_DeactivateReady     ---> []

SemanticAction
    ACT_EnvFresh,           ---> []
    ACT_EnvStore,           ---> Name
    ACT_ReturnBind,         ---> Var / should be VExpr

*/

//--------------------------------------------------------------------------//
// Input Action Definitions
//--------------------------------------------------------------------------//

typedef enum {
    ACT_IEnd,
    ACT_IMatchBytes,
    ACT_Temp_ReadChar
} InputActionType;

typedef struct {
    int withsem;
    Expr* expr;  //TODO: We must use the equivalent of NVExpr here, not generic Expr
} IMatchBytesData ;

typedef struct {
    char chr;
} ReadCharData;

typedef struct {
    InputActionType tag;
    union {
        IMatchBytesData iMatchBytesData;
        ReadCharData readCharData;
    };
} InputAction ;

//--------------------------------------------------------------------------//
// Control Action Definitions
//--------------------------------------------------------------------------//

typedef enum {
    ACT_Push,
    ACT_Pop,
    ACT_ActivateFrame,
    ACT_DeactivateReady
} ControlActionType;

typedef struct {
    char* name;
    //TODO: Expr List?
    int state;
} PushData;

typedef struct {
    //TODO: Do we need a list of names?
    char* name;
} ActivateFrameData;

typedef struct {
    ControlActionType tag;
    union {
        PushData pushData;
        ActivateFrameData activateFrameData;
    };
} ControlAction ;

//--------------------------------------------------------------------------//
// Semantic Action Definitions
//--------------------------------------------------------------------------//

typedef enum {
    ACT_EnvFresh,
    ACT_EnvStore,
    ACT_ReturnBind
} SemanticActionType;

typedef struct {
    char* name;
} EnvStoreData;

typedef struct {
    Expr* expr; //TODO: Should be a VExpr
} ReturnBindData;

typedef struct {
    SemanticActionType tag;
    union {
        EnvStoreData envStoreData;
        ReturnBindData returnBindData;
    };
} SemanticAction ;

//--------------------------------------------------------------------------//
// Branch Action Definitions
//--------------------------------------------------------------------------//

typedef enum {
    ACT_CutBiasAlt
} BranchActionType;

typedef struct {
    int state;
} CutBiasAltData;

typedef struct {
    BranchActionType tag;
    union {
        CutBiasAltData cutBiasAltData;
    };
} BranchAction ;


//--------------------------------------------------------------------------//
// Action Definitions
//--------------------------------------------------------------------------//

typedef enum {
    ACT_EpsA,
    ACT_InputAction,
    ACT_ControlAction,
    ACT_SemanticAction,
    ACT_BranchAction,
} ActionType;

typedef struct {
    ActionType tag;
    union {
        InputAction inputAction;
        ControlAction controlAction;
        SemanticAction semanticAction;
        BranchAction branchAction;
    };
} Action;


//--------------------------------------------------------------------------//
// Functions
//--------------------------------------------------------------------------//

/** Execute an action */
Cfg * applyAction(Action * act, Cfg* cfg, int arrivState);

/** Get a string representation of the action */
char * actionToString(Action * act);

#endif
