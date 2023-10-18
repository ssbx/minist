#ifndef MEM_H
#define MEM_H

struct Memory {
    int   objc;
    char *objs;
};

struct Memory* mem_new();
#endif // MEM_H
