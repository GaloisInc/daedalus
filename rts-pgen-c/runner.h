#ifndef RUNNER_H
#define RUNNER_H

#include <stdio.h>

#include "aut.h"

typedef struct _Input {
    FILE * file;
    fpos_t posInput;
} Input ;

extern void runAut(Aut aut, FILE* input);

#endif /* RUNNER_H */
