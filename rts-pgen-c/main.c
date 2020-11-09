
#include <stdio.h>

#include "runner.h"

extern Aut aut ;

int main(int argc, char ** argv) {
    FILE * input = fopen(argv[1], "r");

    runAut(aut, input);

    exit(0);
}
