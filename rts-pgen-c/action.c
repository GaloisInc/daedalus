#include "action.h"
#include "cfg.h"
#include "aut.h"
#include "debug.h"
#include "util.h"

#include <stdio.h>

//-----------------------------------------------------------------------//
// Function Declarations
//-----------------------------------------------------------------------//

static char * inputActionToString(InputAction* action);

static char * semanticActionToString(SemanticAction* action);

static char * controlActionToString(ControlAction* action);

static Cfg * applyInputAction(InputAction * act, Cfg* cfg, int arrivState);

static Cfg * applyControlAction(ControlAction * act, Cfg* cfg, int arrivState);

static Cfg * applySemanticAction(SemanticAction * act, Cfg* cfg, int arrivState);

//-----------------------------------------------------------------------//
// Action execution
//-----------------------------------------------------------------------//

Cfg * applyInputAction(InputAction * action, Cfg* cfg, int arrivState) {
    switch(action->tag) {
        case ACT_IEnd: {
            //We should be at the end of stream. If not we have an error
            if (!endOfInput(cfg->inp))
                return NULL;

            //Push a default value to the semantic stack and then make a new configuration.
            //TODO: Make sure the semantic value we are pushing on to the stack is correct.
            //Note that the current value type doesn't track with Haskell. Maybe fix?
            Value* defaultValue = createDictValue(createValueDict());
            Cfg * newCfg = mkCfg(
                arrivState, cfg->inp, cfg->ctrl,
                pushStackSem(createValueSemanticElm(defaultValue), cfg->sem)
            );
            return newCfg;
        }
        case ACT_Temp_ReadChar: {
            //Read the next byte from the stream. If it is the expected character,
            //move to the next state, otherwise we have an error.
            fpos_t newPos;
            int c = readInput(cfg->inp, &newPos);
            if (c == action->readCharData.chr) {
               Cfg * newCfg = mkCfg(
                   arrivState, makeNewInput(cfg->inp->file, newPos), cfg->ctrl,
                   pushStackSem(createValueSemanticElm(createIntValue(c)), cfg->sem)
                );
               return newCfg;
            }
            else {
                LOGD("Rejecting in state: %d", cfg->state);
                return NULL;
            }
            break;
        }
        default: {
            LOGD(
                "INPUTACTION (%s) has no explicit implementation. Treating as epsilon temporarily",
                inputActionToString(action)
            );
            Cfg * newCfg = mkCfg(arrivState, cfg->inp, cfg->ctrl, cfg->sem);
            return newCfg;
        }

    }
}

Cfg * applyControlAction(ControlAction * action, Cfg* cfg, int arrivState) {
    switch(action->tag){
        case ACT_Push: {
            //Create a CallFrame instance and push on to the control stack and return a new
            //configuration with the updated stack
            CallFrameElm* elm = ALLOCMEM(sizeof(CallFrameElm));
            elm->state = action->pushData.state;

            ControlElm* newElm = createCallFrameControlElm(elm);
            StackCtrl* newCtrlStack = pushStackCtrl(newElm, cfg->ctrl);

            return mkCfg(arrivState, cfg->inp, newCtrlStack, cfg->sem);
        }
        case ACT_Pop: {
            //We expect to find a state on the control stack for us to pop to
            //TODO: Not finding it seems like a honest-to-goodness internal error and not
            //just a backtrackable failure? Check this up and switch to an ASSERT if it is
            //an error
            if (isEmptyStackCtrl(cfg->ctrl))
                return NULL;

            //Fetch the new state from the stack and create a new configuration
            //reflecting that state.
            ControlElm* topElm = headStackCtrl(cfg->ctrl);
            ASSERT(
                topElm && topElm->tag == CallFrame,
                "Invalid top element during ACT_Pop: %d", topElm->tag
            );

            StackCtrl* newCtrlStack = popStackCtrl(cfg->ctrl);
            if (topElm->callFrameElm->state < 0) {
                return NULL;
            }

            //Return a new configuration that points to the new state and the updated
            //control stack
            return mkCfg(topElm->callFrameElm->state, cfg->inp, newCtrlStack, cfg->sem);
        }
        default: {
            LOGD(
                "CONTROLACTION (%s) has no explicit implementation. Treating as epsilon temporarily",
                controlActionToString(action)
            );
            Cfg * newCfg = mkCfg(arrivState, cfg->inp, cfg->ctrl, cfg->sem);
            return newCfg;
        }
    }
}

Cfg * applySemanticAction(SemanticAction * action, Cfg* cfg, int arrivState) {
    switch(action->tag) {
        case ACT_EnvFresh: {
            SemanticElm* elm = createEnvSemanticElm(createValueDict());
            StackSem* newStack = pushStackSem(elm, cfg->sem);
            return mkCfg(arrivState, cfg->inp, cfg->ctrl, newStack);
        }
        case ACT_EnvStore: {
            //In general environment store creates a new environment.

            //In general it can take a (variable) name as input. It then
            //assumes that the top of the semantic stack is the last stored value and
            //the one above it is an enviroment. It pops those two off the stack and
            //pushes up a new environment that contains the name mapped to the stored
            //value.

            //The top of the stack must be either a regular value or the end. Everything
            //else is an error
            ASSERT_FIELD(cfg->sem, semanticElm, "Top element of semantic stack cannot be empty during ACT_EnvStore");
            ASSERT(
                cfg->sem->semanticElm->tag == SEVal || cfg->sem->semanticElm->tag == SEnd,
                "Invalid top element during ACT_EnvStore: %d", cfg->sem->semanticElm->tag
            );

            //And the second element must be an environment map
            ASSERT_FIELD(cfg->sem->up, semanticElm, "The second element on the semantic stack cannot be empty during ACT_EnvStore");
            ASSERT(
                cfg->sem->up->semanticElm->tag == SEnvMap,
                "Invalid second element during ACT_EnvStore: %d", cfg->sem->semanticElm->tag
            );

            //If the top semantic element is SEnd OR if we are not given a variable to store the last value,
            //it means that the value is being ignored. iWe do that by simply throwing away the top of the
            //semantic stack.
            if (cfg->sem->semanticElm->tag == SEnd || action->envStoreData.name == NULL)
                return mkCfg(arrivState, cfg->inp, cfg->ctrl, cfg->sem->up);

            //So we have a Value at the top of the stack and an environment after that.
            //So do the environment update as described above
            SemanticElm* first = cfg->sem->semanticElm;
            SemanticElm* second = cfg->sem->up->semanticElm;

            Value* storedValue = first->value;
            ValueDict* dictValue = second->dictValue;
            ValueDict* updatedEnv = addDictEntry(action->envStoreData.name, storedValue, dictValue);

            StackSem* stackRoot = cfg->sem->up->up;
            SemanticElm* newElem = createEnvSemanticElm(updatedEnv);
            StackSem* updatedStack = pushStackSem(newElem, stackRoot);

            //Return a new configuration with the updated stack
            return mkCfg(arrivState, cfg->inp, cfg->ctrl, updatedStack);
        }
        case ACT_ReturnBind: {
            //TODO: This implementation DOES NOT track with the Haskell version. Redo soon

            //TODO: We currently only support variables in ReturnBind
            if (action->returnBindData.expr->tag != E_VAR) {
                LOGE("Internal Error: ReturnBind specifies a non-variable. This should not happen currently");
                exit(1);
            }

            //The effect of ReturnBind is to bind the current top of the stack to a specified
            //variable (with the binding is then stored as a dictionary on the stack).

            ASSERT_FIELD(cfg->sem, semanticElm, "Top element of semantic stack cannot be empty during ACT_ReturnBind");
            ASSERT(
                cfg->sem->semanticElm->tag == SEnvMap,
                "Invalid top element during ACT_ReturnBind: %d", cfg->sem->semanticElm->tag
            );

            //Take the variable name from the parameter, bind it to the value popped from
            //the stack, create a new singleton environment with it and  push on to the stack
            Value* v = getDict(action->returnBindData.expr->name, cfg->sem->semanticElm->dictValue);
            SemanticElm* newValue = createValueSemanticElm(v);
            StackSem* updatedStack = pushStackSem(newValue, cfg->sem->up);
            return mkCfg(arrivState, cfg->inp, cfg->ctrl, updatedStack);
        }
        case ACT_ManyFreshList: {
            ValueList* l = createValueList();
            SemanticElm* newElem = createManyValSemanticElm(l);
            StackSem* updatedStack = pushStackSem(newElem, cfg->sem);

            return mkCfg(arrivState, cfg->inp, cfg->ctrl, updatedStack);
        }
        case ACT_ManyAppend: {
            //For ManyAppend, we expect that the top of the stack is the new regular
            //value to store and the second element on the stack is the current "accumulator"
            //(i.e. the list where we are storing intermediate elements - originally created in
            //ACT_ManyFreshList)

            //The top of the stack must be a regular value.
            ASSERT_FIELD(cfg->sem, semanticElm, "Top element of semantic stack cannot be empty during ACT_ManyAppend");
            ASSERT(
                cfg->sem->semanticElm->tag == SEVal,
                "Invalid top element during ACT_ManyAppend: %d", cfg->sem->semanticElm->tag
            );

            //And the second element must be an environment map
            ASSERT_FIELD(cfg->sem->up, semanticElm, "The second element on the semantic stack cannot be empty during ACT_ManyAppend");
            ASSERT(
                cfg->sem->up->semanticElm->tag == SManyVal,
                "Invalid second element during ACT_ManyAppend: %d", cfg->sem->semanticElm->tag
            );

            StackSem* updatedStack = NULL;
            if (action->manyAppendData.withsem) {
                //Push the first element to the front of the list (i.e. the second element)
                SemanticElm* first = cfg->sem->semanticElm;
                SemanticElm* second = cfg->sem->up->semanticElm;

                Value* storedValue = first->value;
                ValueList* listValue = second->listValue;
                ValueList* newListValue = pushListEntry(storedValue, listValue);

                StackSem* stackRoot = cfg->sem->up->up;
                SemanticElm* newElem = createManyValSemanticElm(newListValue);
                updatedStack = pushStackSem(newElem, stackRoot);
            } else {
                updatedStack = cfg->sem;
            }

            //Return a new configuration with the updated stack
            return mkCfg(arrivState, cfg->inp, cfg->ctrl, updatedStack);
        }
        case ACT_ManyReturn: {
            //The top of the stack must be list.
            ASSERT_FIELD(cfg->sem, semanticElm, "Top element of semantic stack cannot be empty during ACT_ManyReturn");
            ASSERT(
                cfg->sem->semanticElm->tag == SManyVal,
                "Invalid top element during ACT_ManyReturn: %d", cfg->sem->semanticElm->tag
            );

            //Extract the list, convert to a *regular* value and push on to stack
            SemanticElm* first = cfg->sem->semanticElm;
            ValueList* lst = first->listValue;

            //TODO: Convert to an array instead of keeping as a list to be in line with Haskell
            StackSem* stackRoot = cfg->sem->up;
            Value* newValue = createListValue(lst);
            SemanticElm* newElem = createValueSemanticElm(newValue);
            StackSem* updatedStack = pushStackSem(newElem, stackRoot);

            //Return a new configuration with the updated stack
            return mkCfg(arrivState, cfg->inp, cfg->ctrl, updatedStack);
        }
        case ACT_DropOneOut: {
            //The semantic stack cannot be empty
            ASSERT_FIELD(cfg->sem, semanticElm, "Top element of semantic stack cannot be empty during ACT_DropOneOut");

            //Return a new configuration after dropping the top of the semantic stack
            return mkCfg(arrivState, cfg->inp, cfg->ctrl, cfg->sem->up);
        }
        default: {
            LOGD(
                "SEMANTICACTION (%s) has no explicit implementation. Treating as epsilon temporarily",
                semanticActionToString(action)
            );
            Cfg * newCfg = mkCfg(arrivState, cfg->inp, cfg->ctrl, cfg->sem);
            return newCfg;
        }
    }
}

Cfg * applyAction(Action * action, Cfg* cfg, int arrivState) {
    LOGD("In execAction - %s", actionToString(action));
    //TODO: printStackSem(cfg->sem);
    switch (action->tag) {
        case ACT_InputAction:
            return applyInputAction(&action->inputAction, cfg, arrivState);
        case ACT_ControlAction:
            return applyControlAction(&action->controlAction, cfg, arrivState);
        case ACT_SemanticAction:
            return applySemanticAction(&action->semanticAction, cfg, arrivState);
        case ACT_EpsA: {
            Cfg * newCfg = mkCfg(arrivState, cfg->inp, cfg->ctrl, cfg->sem);
            return newCfg;
        }
        default: {
            LOGD(
                "ACTION (%s) has no explicit implementation. Treating as epsilon temporarily",
                actionToString(action)
            );
            Cfg * newCfg = mkCfg(arrivState, cfg->inp, cfg->ctrl, cfg->sem);
            return newCfg;
        }
    }
}

//-----------------------------------------------------------------------//
// Pretty Printing
//-----------------------------------------------------------------------//

char * inputActionToString(InputAction* action) {
    switch (action->tag) {
        case ACT_IEnd:
            return "IEnd";
        case ACT_IMatchBytes:
            return "IMatchBytes";
        case ACT_Temp_ReadChar:
        {
            char* text = ALLOCMEM(100);
            snprintf(text, 100, "ReadChar: %c", action->readCharData.chr);
            return text;
        }
        default:
            return "<UnknownInputAction>";
    }
}

char * controlActionToString(ControlAction* action) {
    switch (action->tag) {
        case ACT_Push:
            return "Push";
        case ACT_Pop:
            return "Pop";
        case ACT_ActivateFrame:
            return "ActivateFrame";
        case ACT_DeactivateReady:
            return "DeactivateReady";
        case ACT_BoundSetup:
            return "BoundSetup";
        case ACT_BoundCheckSuccess:
            return "BoundCheckSuccess";
        case ACT_BoundIsMore:
            return "BoundIsMore";
        case ACT_BoundIncr:
            return "BoundIncr";
        default:
            return "<UnknownControlAction>";
    }
}

char * semanticActionToString(SemanticAction* action) {
    switch (action->tag) {
        case ACT_EnvFresh:
            return "EnvFresh";
        case ACT_EnvStore:
            return "EnvStore";
        case ACT_ReturnBind:
            return "ReturnBind";
        case ACT_ManyFreshList:
            return "ManyFreshList";
        case ACT_ManyAppend:
            return "ManyAppend";
        case ACT_ManyReturn:
            return "ManyReturn";
        case ACT_DropOneOut:
            return "DropOneOut";
        default:
            return "<UnknownSemanticAction>";
    }
}

char * actionToString(Action * act) {
    switch (act->tag) {
        case ACT_EpsA:
            return "EpsA" ;
        case ACT_InputAction:
            return inputActionToString(&act->inputAction);
        case ACT_ControlAction:
            return controlActionToString(&act->controlAction);
        case ACT_SemanticAction:
            return semanticActionToString(&act->semanticAction);
        case ACT_BranchAction:
            LOGE("illegal BranchAction execution");
            exit(1);
        default:
            return NULL;
    }
}
