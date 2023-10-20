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
    PUSH_LITERAL_CONSTANT,
    PUSH_LITERAL_VARIABLE,
    POP_STORE_RCVR,
    POP_STORE_TEMP,
    PUSH_CONSTANT,
    RETURN_CONSTANT,
    RETURN_STACK_TOP_FROM,
    SEND_LITERAL_NOARG,
    SEND_LITERAL_1ARG,
    SEND_LITERAL_2ARG,
    SEND_BIN_MSG
};

const char*   bytecodes_getBytecodeDescription(unsigned char);
unsigned char bytecodes_getCodeFor(int,int);

#endif // BYTECODES_H
