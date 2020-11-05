#include "aut.h"

char * action_to_string(Action * act) {
    switch (act->tag) {
        case ACT_EpsA : {
            return "EpsA" ;
        }
        case ACT_Match: {
            return "Match" ;
        }
        case ACT_ReadChar: {
            return &(act->chr);
            break;
        }
        case ACT_Push: {
            return "Push";
            break;
        }
        case ACT_Pop: {
            return "Pop";
            break;
        }
        case ACT_ReturnBind: {
            return "ReturnBind";
        }
        case ACT_EnvFresh: {
            return "EnvFresh";
        }
        case ACT_EnvStore: {
            //char s[256];
            //strcpy()
            return "EnvStore";
        }
        case ACT_ActivateFrame: {
            return "ActivateFrame";
        }
        case ACT_DeactivateReady: {
            return "DeactivateReady";
        }
        case ACT_END: {
            return "END";
            break;
        }
        default: {
            return NULL;
        }
    }
}
