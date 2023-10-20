#include "utils.h"
#include <stddef.h>
#include <string.h>
#include <stdlib.h>

char *strdup2(const char *str) {
    size_t len = strlen(str) + 1;
    char  *new = malloc(len);
    memcpy(new, str, len);
    return new;
}
