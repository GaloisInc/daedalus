#include "runner.h"
#include "value.h"

//-----------------------------------------------------------------------//
// Runtime Library: Configurations
//-----------------------------------------------------------------------//

typedef struct _StackSem {
    Value * value;
    struct _StackSem* up;
} StackSem;

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

StackSem * updateStackSem(Value * v, StackSem *stack) {
    StackSem * st = malloc(sizeof(StackSem));
    st->value = v;
    st->up = stack;

    return st;
}

void printStackSem(StackSem * stack) {
    //Value * value;
    //struct _StackSem* up;
    printf("STACK_SEM: ");
    while (stack != NULL && stack->value != NULL){
        printf("Elm: ");
        print_Value(stack->value);
        stack = stack->up;
    }
    printf("\n");
}

Input * initInput(FILE * f){
    Input * inp = malloc(sizeof(Input));
    inp->file = f;
    inp->posInput = 0;

    return inp;
}

int endOfInput(Input* input) {
    fsetpos(input->file, &input->posInput);

    int c = fgetc(input->file);
    if (c == EOF) {
        return 1;
    } else {
        ungetc(c, input->file);
        return 0;
    }
}

typedef struct _StackCtrl {
    int state;
    struct _StackCtrl * up;
} StackCtrl;

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

typedef struct _Cfg {
    int state;
    Input * inp;
    StackCtrl * ctrl;
    StackSem * sem;
    struct _Cfg* next;
} Cfg;

Cfg * mkCfg(int state, Input * inp, StackCtrl * ctrl, StackSem * sem){
    Cfg* cfg = malloc(sizeof(Cfg));
    cfg->state = state;
    cfg->inp = inp;
    cfg->ctrl = ctrl;
    cfg->sem = sem;
    cfg->next = NULL;

    return cfg;
}


//-----------------------------------------------------------------------//
// Runtime Library: Action execution
//-----------------------------------------------------------------------//


//Reads a character. Also return the next pos
int readInput(Input* input, fpos_t* newPos) {
    fsetpos(input->file, &input->posInput);
    int c = fgetc(input->file);
    fgetpos(input->file, newPos);
    return c;
}

Input* makeNewInput(FILE* file, fpos_t pos) {
    Input* newInput = malloc(sizeof(Input));
    newInput->file = file;
    newInput->posInput = pos;

    return newInput;
}

Cfg * execAction(Action * act, Cfg* cfg, int arrivState){
    printf("execAction\n");
    printStackSem(cfg->sem);
    switch (act->tag) {
        case ACT_END: {
            fpos_t newPos;
            int c = readInput(cfg->inp, &newPos);
            if (EOF == c) {
               Cfg * newCfg = mkCfg(arrivState, makeNewInput(cfg->inp->file, newPos), cfg->ctrl, updateStackSem(empty_dict(), cfg->sem));
               return newCfg;
            }
            else {
                return NULL;
            }
            break;
        }
        case ACT_ReadChar: {
            fpos_t newPos;
            int c = readInput(cfg->inp, &newPos);
            if (c == act->chr) {
               Cfg * newCfg = mkCfg(arrivState, makeNewInput(cfg->inp->file, newPos), cfg->ctrl, updateStackSem(create_value(c), cfg->sem));
               return newCfg;
            }
            else {
                printf("Rejecting in state: %d\n", cfg->state);
                return NULL;
            }
            break;
        }
        case ACT_Push: {
            StackCtrl* newCtrlStack = pushStackCtrl(act->state, cfg->ctrl);
            return mkCfg(arrivState, cfg->inp, newCtrlStack, cfg->sem);
        }
        case ACT_Pop: {
            if (isEmptyStackCtrl(cfg->ctrl))
                return NULL;

            StackCtrl* newCtrlStack = popStackCtrl(cfg->ctrl);
            if (act->state != cfg->ctrl->state) {
                return NULL;
            }

            return mkCfg(arrivState, cfg->inp, newCtrlStack, cfg->sem);
        }
        case ACT_EnvFresh: {
            return
                mkCfg(
                    arrivState,
                    cfg->inp,
                    cfg->ctrl,
                    updateStackSem(
                        empty_dict(),
                        cfg->sem
                    )
                );
        }
        case ACT_EnvStore: {

            if (act->name == NULL)
                return mkCfg(
                    arrivState,
                    cfg->inp,
                    cfg->ctrl,
                    cfg->sem->up
                );
            
            return
                mkCfg(
                    arrivState,
                    cfg->inp,
                    cfg->ctrl,
                    updateStackSem(
                        add_entry_dict(
                            act->name,
                            cfg->sem->value,
                            cfg->sem->up->value
                        ),
                        cfg->sem->up->up
                    )
                );
        }
        case ACT_EpsA: {
            Cfg * newCfg = mkCfg(arrivState, cfg->inp, cfg->ctrl, cfg->sem);
            return newCfg;
        }
        case ACT_ReturnBind: {
            if (act->expr->tag != E_VAR) {
                printf("SHOULD NOT HAPPEN\n");
                exit(1);
            }
            return
                mkCfg(
                    arrivState,
                    cfg->inp,
                    cfg->ctrl,
                    updateStackSem(
                        get_dict(act->expr->name, cfg->sem->value),
                        cfg->sem->up
                    )
                );
        }
        default: {
          // printf("Abort: ACTION NOT HANDLED");
          // exit(1);
          //Treat as Epsion for now
          Cfg * newCfg = mkCfg(arrivState, cfg->inp, cfg->ctrl, cfg->sem);
          return newCfg;
        }
    }
}


//-----------------------------------------------------------------------//
// Runtime Library: Naive Backtracking Runner
//-----------------------------------------------------------------------//


typedef struct _Stack {
    int state;
    int pos;
    Cfg * cfg;
    struct _Stack* up;
} Stack;

Stack * initStack(Cfg * cfg) {
    return NULL;
}

Stack * pushStack(int state, int pos, Cfg * cfg, Stack *stack) {
    Stack * st = malloc(sizeof(Stack));
    st->state = state;
    st->pos = pos;
    st->cfg = cfg;
    st->up = stack;

    return st;
}

typedef struct _Result {
    Value * value;
    struct _Result * next;
} Result;

Result * initResult() {
    return NULL;
}

Result * addResult(Value * v, Result * res){
    Result * r = malloc(sizeof(Result));
    r->value = v;
    r->next = res;

    return r;
}

void print_Result(Result * r){

    printf("Results:\n");
    int i = 0;
    while (r != NULL) {
        i++;
        printf("#%d ", i);
        print_Value(r->value);
        r = r->next;
        printf("\n");
    }
    return;
}

Result * runner(Aut aut, Cfg * cfg , Stack * st, Result * r);
Result * backtrack(Aut aut, Stack * st, Result * r);
Result * step(Aut aut, Action * act, int arrivState, Stack * st, Result * r);

Result * runner(Aut aut, Cfg * cfg , Stack * st, Result * r) {
    printf("STATE: %d\n", cfg->state);
    //printf("Stack at: %p\n", cfg->ctrl);

    Result * new_res = r;

    if (cfg->ctrl == NULL && cfg->state == aut.accepting) {
        new_res = addResult(cfg->sem->value, r);
        printf("Solution found!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!\n");
    }

    Choice ch = aut.table[cfg->state];
    ActionStatePair* tr = ch.transitions;

    Stack * newStack;
    printf("Tag: %d\n", ch.tag);
    switch (ch.tag) {
        case UNICHOICE: {
            Action* act = tr[0].pAction;
            int arrivalState = tr[0].state;

            newStack = pushStack(cfg->state, 1, cfg, st);
            return step(aut, act, arrivalState, newStack, new_res);
            break;
        };
        case PARCHOICE: {
            Action * act = tr[0].pAction;
            int arrivalState = tr[0].state;

            newStack = pushStack(cfg->state, 1, cfg, st);
            return step(aut, act, arrivalState, newStack, new_res);
            break;
        };
        case SEQCHOICE: {
            //TODO:
        }
        case EMPTYCHOICE: {
            printf("EMPTYCHOICE\n");
            return backtrack(aut, st, new_res);
            break;
        }
        default: {
            printf("Exited\n");
            exit(1);
        };
    }
}

Result * step(Aut aut, Action *act, int arrivState, Stack *st, Result * r) {
    printf("STEP: action:%s arrivState:%d\n", action_to_string(act), arrivState);

    Cfg* cfg = st->cfg;
    Cfg* newCfg = NULL;
    newCfg = execAction(act, cfg, arrivState);

    if (newCfg == NULL) {
        return backtrack(aut, st, r);
    } else {
        return runner(aut, newCfg, st, r);
    }
}

Result * backtrack(Aut aut, Stack *st, Result * res){
    printf("BACKTRACK\n");
    if (st == NULL) {
        return res;
    } else {
        int state = st->state;
        int pos = st->pos;
        Stack* upStack = st->up;

        if (pos >= aut.table[state].len) {
            return backtrack(aut, upStack, res);
        } else {
            Stack * newStack = pushStack(state, pos+1, st->cfg, upStack);
            return step(
                aut,
                aut.table[state].transitions[pos].pAction,
                aut.table[state].transitions[pos].state,
                newStack,
                res
            );
        }
    }
}

void runAut(Aut aut, FILE* input) {
    Result * res = runner(
        aut,
        mkCfg(aut.initial, initInput(input), initStackCtrl(), initStackSem()),
        initStack(NULL),
        initResult()
    );

    print_Result(res);
    printf("\n");
}
