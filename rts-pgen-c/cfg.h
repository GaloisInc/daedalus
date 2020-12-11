#ifndef CFG_H
#define CFG_H

#include <stdio.h>

#include "value.h"

/** An Input is a pair consisting of a FILE and a position */
typedef struct _Input {
    FILE * file;
    fpos_t posInput;
} Input ;

/** The semantic stack - essentially a stack of the currently available semantic values */
typedef struct _StackSem {
    Value * value;
    struct _StackSem* up;
} StackSem;

/** The control stack that models a "call-stack" of grammar rules being executed */
typedef struct _StackCtrl {
    int state;
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

/** Initialize a semantic stack */
StackSem * initStackSem();

/** Push a value on to the semantic stack */
StackSem * pushStackSem(Value * v, StackSem *stack);

//** Print the semantic stack */
void printStackSem(StackSem * stack);

/** Initialize a control stack */
StackCtrl *  initStackCtrl();

/** Is the control stack empty */
int isEmptyStackCtrl(StackCtrl * ctrl);

/** Push a state on to a control stack */
StackCtrl * pushStackCtrl(int state, StackCtrl *stack);

/** Pop the top from the control stack */
StackCtrl * popStackCtrl(StackCtrl *stack);

/** Peek at the top of the control stack */
int headStackCtrl(StackCtrl *stack);


#endif /* CFG_H_ */