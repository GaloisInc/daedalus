#include "action.h"
#include "cfg.h"
#include "aut.h"
#include "debug.h"

#include <stdio.h>

//-----------------------------------------------------------------------//
// Action execution
//-----------------------------------------------------------------------//

Cfg * execAction(Action * act, Cfg* cfg, int arrivState) {
    LOGD("In execAction - %s", action_to_string(act));
    //TODO: printStackSem(cfg->sem);
    switch (act->tag) {
        case ACT_END: {
            fpos_t newPos;

            if (endOfInput(cfg->inp)) {
               Cfg * newCfg = mkCfg(arrivState, makeNewInput(cfg->inp->file, newPos), cfg->ctrl, pushStackSem(empty_dict(), cfg->sem));
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
               Cfg * newCfg = mkCfg(arrivState, makeNewInput(cfg->inp->file, newPos), cfg->ctrl, pushStackSem(create_value(c), cfg->sem));
               return newCfg;
            }
            else {
                LOGD("Rejecting in state: %d", cfg->state);
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

            int newState = headStackCtrl(cfg->ctrl);
            StackCtrl* newCtrlStack = popStackCtrl(cfg->ctrl);
            if (newState < 0) {
                return NULL;
            }

            return mkCfg(newState, cfg->inp, newCtrlStack, cfg->sem);
        }
        case ACT_EnvFresh: {
            return
                mkCfg(
                    arrivState,
                    cfg->inp,
                    cfg->ctrl,
                    pushStackSem(
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
                    pushStackSem(
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
                LOGE("Internal Error: ReturnBind specifies a non-variable. This should not happen currently");
                exit(1);
            }
            return
                mkCfg(
                    arrivState,
                    cfg->inp,
                    cfg->ctrl,
                    pushStackSem(
                        get_dict(act->expr->name, cfg->sem->value),
                        cfg->sem->up
                    )
                );
        }
        default: {
            LOGD(
                "ACTION (%s) has no explicit implementation. Treating as epsilon temporarily",
                action_to_string(act)
            );
            Cfg * newCfg = mkCfg(arrivState, cfg->inp, cfg->ctrl, cfg->sem);
            return newCfg;
        }
    }
}
