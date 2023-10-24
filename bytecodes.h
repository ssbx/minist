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
    STORE,
    POP_STORE,
    RETURN_CONSTANT,
    RETURN_STACK_TOP_FROM,
    SEND_LITERAL_NOARG,
    SEND_LITERAL_1ARG,
    SEND_LITERAL_2ARG,
    SEND_BIN_MSG,

    /* STORE extention byte */
    STORE_EXT_RCVR,
    STORE_EXT_TEMP,
    STORE_EXT_LITERAL_CONST,
    STORE_EXT_LITERAL_VAR,

    /* POP_STORE extention byte */
    POP_STORE_EXT_RCVR,
    POP_STORE_EXT_TEMP,
    POP_STORE_EXT_LITERAL_CONST,
    POP_STORE_EXT_LITERAL_VAR

};

int bytecodes_needsExtent(unsigned char);
const char*   bytecodes_getBytecodeDescription(unsigned char);
const char*   bytecodes_getExtBytecodeDescription(unsigned char, unsigned char);
unsigned char bytecodes_getCodeFor(int,int);
unsigned char bytecodes_getExtendedCodeFor(int,int);

#endif // BYTECODES_H
