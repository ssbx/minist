#include "utils.h"
#include <stddef.h>
#include <string.h>
#include <stdlib.h>

char *strdup2(const char *old) {
    size_t len = strlen(old) + 1;
    char  *new = malloc(len);
    memcpy(new, old, len);
    return new;
}
