#ifndef ACTION_H
#define ACTION_H

#include <stdio.h>

#include "aut.h"
#include "cfg.h"

/** Execute an action */
Cfg * execAction(Action * act, Cfg* cfg, int arrivState);

#endif