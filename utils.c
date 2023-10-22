#include "utils.h"
#include <stddef.h>
#include <string.h>
#include <stdlib.h>

char *strdup2(const char *old) {
    size_t len = strlen(old);
    char  *new = malloc(len + 1);
    memcpy(new, old, len);
    return new;
}
