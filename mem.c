#include <stdlib.h>
#include "mem.h"

struct Memory* mem_new() {
    struct Memory* m = malloc(sizeof(struct Memory));
    m->objc = 0;
    m->objs = NULL;
    return m;
}
