#include "cfg.h"
#include "debug.h"

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
    Input* newInput = malloc(sizeof(Input));
    newInput->file = file;
    newInput->posInput = pos;

    return newInput;
}

//-----------------------------------------------------------------------//
// Semantic Stack Management
//-----------------------------------------------------------------------//

StackSem * initStackSem() {
    StackSem * st = NULL;
    return st;
}

StackSem * pushStackSem(Value * v, StackSem *stack) {
    StackSem * st = malloc(sizeof(StackSem));
    st->value = v;
    st->up = stack;

    return st;
}

void printStackSem(StackSem * stack) {
    printf("STACK_SEM: ");
    while (stack != NULL && stack->value != NULL){
        printf("Elm: ");
        printValue(stack->value);
        stack = stack->up;
    }
    printf("\n");
}

//-----------------------------------------------------------------------//
// Control Stack Management
//-----------------------------------------------------------------------//

StackCtrl *  initStackCtrl() {
    return NULL;
}

int isEmptyStackCtrl(StackCtrl * ctrl) {
    return ctrl == NULL;
}

StackCtrl * pushStackCtrl(int state, StackCtrl *stack) {
    StackCtrl* st = malloc(sizeof(StackCtrl));
    st->state = state;
    st->up = stack;

    return st;
}

StackCtrl * popStackCtrl(StackCtrl *stack) {
    //printf("%p\n", stack);

    StackCtrl* up = stack->up;
    //free(stack); - We may not be able to do this because the stack can be shared in case of multiple simultaneous Cfgs

    return up;
}

int headStackCtrl(StackCtrl *stack){
   return stack->state;
}

void printStackCtrl(StackCtrl * stack) {
    fprintf(stderr, "STACK_CTRL: ");
    while (stack != NULL){
        fprintf(stderr, "%d", stack->state);
        stack = stack->up;
        fprintf(stderr, ", ");
    }
    fprintf(stderr,"\n");
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
    printStackCtrl(cfg->ctrl);
    return (cfg->ctrl == NULL && cfg->state == acceptingState) ;
}
