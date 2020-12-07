#include "runner.h"
#include "value.h"
#include "action.h"
#include "cfg.h"

#include "debug.h"

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
        if (r != NULL)
            printf("\n");
    }
    return;
}

Result * runner(Aut aut, Cfg * cfg , Stack * st, Result * r);
Result * backtrack(Aut aut, Stack * st, Result * r);
Result * step(Aut aut, Action * act, int arrivState, Stack * st, Result * r);

Result * runner(Aut aut, Cfg * cfg , Stack * st, Result * r) {
    LOGD("STATE: %d", cfg->state);
    //printf("Stack at: %p\n", cfg->ctrl);

    Result * new_res = r;

    if (isEmptyStackCtrl(cfg->ctrl) && cfg->state == aut.accepting) {
        new_res = addResult(cfg->sem->value, r);
        LOGI("Solution found!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!");
    }

    Choice ch = aut.table[cfg->state];
    ActionStatePair* tr = ch.transitions;

    Stack * newStack;
    LOGD("Tag: %d", ch.tag);
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
            LOGD("EMPTYCHOICE");
            //Action pop = MAKE_ACT_POP();
            int arrivalState = -42;
            Action act = { .tag = ACT_ControlAction, .controlAction = { .tag = ACT_Pop} };

            //newStack = pushStack(cfg->state, 1, cfg, st);
            newStack = pushStack(arrivalState, 1, cfg, st);
            return step(aut, &act, arrivalState, newStack, new_res);
            break;
        }
        default: {
            printf("Exited\n");
            exit(1);
        };
    }
}

Result * step(Aut aut, Action *act, int arrivState, Stack *st, Result * r) {
    LOGD("STEP: action:%s arrivState:%d", actionToString(act), arrivState);

    Cfg* cfg = st->cfg;
    Cfg* newCfg = NULL;
    newCfg = applyAction(act, cfg, arrivState);

    if (newCfg == NULL) {
        return backtrack(aut, st, r);
    } else {
        return runner(aut, newCfg, st, r);
    }
}

Result * backtrack(Aut aut, Stack *st, Result * res){
    LOGD("BACKTRACK");
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
