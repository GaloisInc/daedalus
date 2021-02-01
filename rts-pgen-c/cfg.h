#ifndef CFG_H
#define CFG_H

#include <stdio.h>

#include "value.h"

/** An Input is a pair consisting of a FILE and a position */
typedef struct _Input {
    FILE * file;
    fpos_t posInput;
} Input ;

/** Semantic stack value type */
typedef struct _SemanticElm {
    enum { SEnvMap, SEVal, SManyVal, SEnd } tag;
    union {
        ValueDict* dictValue;
        Value* value;
        ValueList* listValue;
    };
} SemanticElm ;

/** The semantic stack - essentially a stack of the currently available semantic values */
typedef struct _StackSem {
    SemanticElm * semanticElm;
    struct _StackSem* up;
} StackSem;

typedef struct {
    int value;
} CExactlyData;

/**
 * This represents the CBetween constructor in the Haskell runtime. Note that
 * `None` values for left and right are represented with -1 in C since these values
 * are otherwise positive from what I know.
 *
 * If they can indeed be negative, we need to add flags here to represent the validity of
 * the bounds.
 */
typedef struct {
    int left;
    int right;
} CBetweenData;

/** Control Stack element for handling Many */
typedef struct {
    enum { CExactly, CBetween } tag;
    union {
        CExactlyData* cExactlyData;
        CBetweenData* cBetweenData;
    };
    int current;
} ManyFrameElm;

/** Control Stack element for handling For */
typedef struct {
    //TODO:
} ForFrameElm;

/** Control Stack element for handling Map */
typedef struct {
    //TODO:
} MapFrameElm;

/** Control Stack element for handling Calls */
typedef struct {
    int state;
    //TODO: Rest of the fields - data Name, ActivationFrame and SemanticData
} CallFrameElm;

/** The control stack element */
typedef struct {
    enum { ManyFrame, ForFrame, MapFrame, CallFrame } tag;
    union {
        ManyFrameElm* manyFrameElm;
        ForFrameElm* forFrameElm;
        MapFrameElm* mapFrameElm;
        CallFrameElm* callFrameElm;
    };
} ControlElm;

/** The control stack that models a "call-stack" of grammar rules being executed */
typedef struct _StackCtrl {
    ControlElm* controlElm;
    struct _StackCtrl * up;
} StackCtrl;

/**
 * A configuration that represents the state of the running automata
 * Ideally this should be defined independent of Action (say in) Cfg.h. However actions operate
 * on (components of) Cfg and
 */
typedef struct _Cfg {
    int state;
    Input * inp;
    StackCtrl * ctrl;
    StackSem * sem;
    struct _Cfg* next;
} Cfg;

/** Create a configuration instance */
Cfg * mkCfg(int state, Input * inp, StackCtrl * ctrl, StackSem * sem);

/** Is this configuration at an accepting state and situation */
int isAcceptingCfg(Cfg* cfg, int acceptingState);

/** Initialize Input from a FILE */
Input * initInput(FILE * f);

/** Check if at the end of input */
int endOfInput(Input* input);

/** Read a byte from the input */
int readInput(Input* input, fpos_t* newPos);

/** Make a new input instance */
Input* makeNewInput(FILE* file, fpos_t pos);

/** Create semantic element from a dictionary */
SemanticElm* createEnvSemanticElm(ValueDict* d);

/** Create semantic element from a regular value */
SemanticElm* createValueSemanticElm(Value* v);

/** Create semantic element from a list */
SemanticElm* createManyValSemanticElm(ValueList* l);

/** Initialize a semantic stack */
StackSem * initStackSem();

/** Push a semantic element to the semantic stack */
StackSem * pushStackSem(SemanticElm* elm, StackSem * stack);

/** Print the semantic stack */
void printStackSem(StackSem * stack);

/** Create a control element for Many */
ControlElm* createManyFrameControlElm(ManyFrameElm* mf);

/** Create a control element for the For loop */
ControlElm* createForFrameControlElm(ForFrameElm* ff);

/** Create a control element for the Map */
ControlElm* createMapFrameControlElm(MapFrameElm* mf);

/** Create a control element for the Call */
ControlElm* createCallFrameControlElm(CallFrameElm* mf);

/** Initialize a control stack */
StackCtrl *  initStackCtrl();

/** Is the control stack empty */
int isEmptyStackCtrl(StackCtrl * ctrl);

/** Push a state on to a control stack */
StackCtrl * pushStackCtrl(ControlElm* elm, StackCtrl *stack);

/** Pop the top from the control stack */
StackCtrl * popStackCtrl(StackCtrl *stack);

/** Print the control stack */
void printStackCtrl(StackCtrl * stack);

/** Peek at the top of the control stack */
ControlElm* headStackCtrl(StackCtrl *stack);


#endif /* CFG_H_ */