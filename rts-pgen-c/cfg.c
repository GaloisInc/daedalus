#include "cfg.h"
#include "debug.h"
#include "util.h"

#include <stdlib.h>

//-----------------------------------------------------------------------//
// Input Management
//-----------------------------------------------------------------------//

//Create an input instance from a file
Input * initInput(FILE * f){
    Input * inp = malloc(sizeof(Input));
    inp->file = f;
    fgetpos(f, &inp->posInput);

    return inp;
}

//Check if we are at the end of input
int endOfInput(Input* input) {
    fsetpos(input->file, &input->posInput);

    int c = fgetc(input->file);
    if (feof(input->file)) {
        return 1;
    } else {
        ungetc(c, input->file);
        return 0;
    }
}

//Reads a character. Also return the next pos
int readInput(Input* input, fpos_t* newPos) {
    fsetpos(input->file, &input->posInput);
    int c = fgetc(input->file);
    fgetpos(input->file, newPos);
    LOGD("READINPUT %d", (int) c);
    return c;
}

//Create a new Input instance given a file and a position
Input* makeNewInput(FILE* file, fpos_t pos) {
    Input* newInput = ALLOCMEM(sizeof(Input));
    newInput->file = file;
    newInput->posInput = pos;

    return newInput;
}

//-----------------------------------------------------------------------//
// Semantic Stack Management
//-----------------------------------------------------------------------//

SemanticElm* createEnvSemanticElm(ValueDict* d) {
    SemanticElm* elm = ALLOCMEM(sizeof(SemanticElm));
    elm->tag = SEnvMap;
    elm->dictValue = d;
    return elm;
}

SemanticElm* createValueSemanticElm(Value* v) {
    SemanticElm* elm = ALLOCMEM(sizeof(SemanticElm));
    elm->tag = SEVal;
    elm->value = v;
    return elm;
}

SemanticElm* createManyValSemanticElm(ValueList* l) {
    SemanticElm* elm = ALLOCMEM(sizeof(SemanticElm));
    elm->tag = SManyVal;
    elm->listValue = l;
    return elm;
}

StackSem * initStackSem() {
    StackSem * st = NULL;
    return st;
}

StackSem * pushStackSem(SemanticElm * elm, StackSem *stack) {
    StackSem * st = malloc(sizeof(StackSem));
    st->semanticElm = elm;
    st->up = stack;

    return st;
}

void printStackSem(StackSem * stack) {
    printf("STACK_SEM: ");
    while (stack != NULL && stack->semanticElm != NULL){
        printf("Elm: ");
        switch(stack->semanticElm->tag) {
            case SEnvMap:
                printValueDict(stack->semanticElm->dictValue);
                break;
            case SEVal:
                printValue(stack->semanticElm->value);
                break;
            case SManyVal:
                printValueList(stack->semanticElm->listValue);
                break;
            case SEnd:
                printf("END");
                break;
            default:
                break;
        }
        stack = stack->up;
        if (stack != NULL)
            printf(" ");
    }
    printf("\n");
}

//-----------------------------------------------------------------------//
// Control Stack Management
//-----------------------------------------------------------------------//

ControlElm* createManyFrameControlElm(ManyFrameElm* mf) {
    ControlElm* elm = ALLOCMEM(sizeof(ControlElm));
    elm->tag = ManyFrame;
    elm->manyFrameElm = mf;
    return elm;
}

/** Create a control element for the For loop */
ControlElm* createForFrameControlElm(ForFrameElm* ff) {
    ControlElm* elm = ALLOCMEM(sizeof(ControlElm));
    elm->tag = ForFrame;
    elm->forFrameElm = ff;
    return elm;
}

/** Create a control element for the Map */
ControlElm* createMapFrameControlElm(MapFrameElm* mf) {
    ControlElm* elm = ALLOCMEM(sizeof(ControlElm));
    elm->tag = MapFrame;
    elm->mapFrameElm = mf;
    return elm;
}

/** Create a control element for the Call */
ControlElm* createCallFrameControlElm(CallFrameElm* cf) {
    ControlElm* elm = ALLOCMEM(sizeof(ControlElm));
    elm->tag = CallFrame;
    elm->callFrameElm = cf;
    return elm;
}

StackCtrl *  initStackCtrl() {
    return NULL;
}

int isEmptyStackCtrl(StackCtrl * ctrl) {
    return ctrl == NULL;
}

StackCtrl * pushStackCtrl(ControlElm* elm, StackCtrl *stack) {
    StackCtrl* st = malloc(sizeof(StackCtrl));
    st->controlElm = elm;
    st->up = stack;

    return st;
}

StackCtrl * popStackCtrl(StackCtrl *stack) {
    ASSERT(stack != NULL, "Attempt to pop empty control stack");
    return stack->up;
}

ControlElm* headStackCtrl(StackCtrl *stack){
    if (stack == NULL)
        return NULL;
    return stack->controlElm;
}

void printManyFrameElm(ManyFrameElm* elm) {
    if (elm == NULL)
        return;

    switch(elm->tag) {
        case CExactly:
            printf("{ exactly: %d, current: %d }", elm->cExactlyData->value, elm->current);
            break;
        case CBetween:
            printf(
                "{ between: %d - %d, current: %d }",
                elm->cBetweenData->left, elm->cBetweenData->right,
                elm->current
            );
            break;
        default:
            break;
    }
}

void printForFrameElm(ForFrameElm* elm) {
    //TODO
}

void printMMapFrameElm(MapFrameElm* elm) {
    //TODO:
}

void printCallFrameElm(CallFrameElm* elm) {
    if (elm == NULL)
        return;
    printf("{ State: %d }", elm->state);
}

void printStackCtrl(StackCtrl * stack) {
    printf("STACK_CTRL: ");
    while (stack != NULL && stack->controlElm != NULL){
        printf("Elm: ");
        switch(stack->controlElm->tag) {
            case ManyFrame:
                printManyFrameElm(stack->controlElm->manyFrameElm);
                break;
            case ForFrame:
                printManyFrameElm(stack->controlElm->forFrameElm);
                break;
            case MapFrame:
                printManyFrameElm(stack->controlElm->mapFrameElm);
                break;
            case CallFrame:
                printManyFrameElm(stack->controlElm->callFrameElm);
                break;
            default:
                break;
        }
        stack = stack->up;
        if (stack != NULL)
            printf(" ");
    }
    printf("\n");
}

//-----------------------------------------------------------------------//
// Configuration Management
//-----------------------------------------------------------------------//

Cfg * mkCfg(int state, Input * inp, StackCtrl * ctrl, StackSem * sem)
{
    Cfg* cfg = malloc(sizeof(Cfg));
    cfg->state = state;
    cfg->inp = inp;
    cfg->ctrl = ctrl;
    cfg->sem = sem;
    cfg->next = NULL;

    return cfg;
}

int isAcceptingCfg(Cfg* cfg, int acceptingState) {
    //printStackCtrl(cfg->ctrl);
    return (cfg->ctrl == NULL && cfg->state == acceptingState) ;
}
