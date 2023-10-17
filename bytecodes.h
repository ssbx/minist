#ifndef BYTECODES_H
#define BYTECODES_H

enum {
    STACK_BYTECODE,
    JUMP_BYTECODE,
    SEND_BYTECODE,
    RETURN_BYTECODE
};

enum {
    PUSH_RCVR,
    PUSH_TEMP,
    PUSH_CONSTANT,
    SEND_BIN_MSG
};

const char*   bytecodes_getBytecodeDescription(unsigned char);
unsigned char bytecodes_getCodeFor(int,int);

#endif // BYTECODES_H
