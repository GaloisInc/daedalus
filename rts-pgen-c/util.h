#include <stdlib.h>

// Memory management macros

/** Allocate memory */
#define ALLOCMEM(size) (malloc(size))

/** Deallocate memory */
#define DEALLOCMEM(ptr) (free(ptr))