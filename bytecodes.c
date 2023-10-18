#include "bytecodes.h"
#include "parse.tab.h"
#include <assert.h>

const char* bytecodes_getBytecodeDescription(unsigned char code) {
    if (code == 0) return "pushRcvr: 0";
    if (code == 1) return "pushRcvr: 1";
    if (code == 2) return "pushRcvr: 2";
    if (code == 3) return "pushRcvr: 3";
    if (code == 4) return "pushRcvr: 4";
    if (code == 5) return "pushRcvr: 5";
    if (code == 6) return "pushRcvr: 6";
    if (code == 7) return "pushRcvr: 7";
    if (code == 8) return "pushRcvr: 8";
    if (code == 9) return "pushRcvr: 9";
    if (code == 10) return "pushRcvr: 10";
    if (code == 11) return "pushRcvr: 11";
    if (code == 12) return "pushRcvr: 12";
    if (code == 13) return "pushRcvr: 13";
    if (code == 14) return "pushRcvr: 14";
    if (code == 15) return "pushRcvr: 15";
    if (code == 176) return "send: +";
    if (code == 177) return "send: -";
    if (code == 178) return "send: <";
    if (code == 179) return "send: >";
    if (code == 180) return "send: <=";
    if (code == 181) return "send: >=";
    if (code == 182) return "send: =";
    if (code == 183) return "send: !=+";
    if (code == 184) return "send: *";
    if (code == 185) return "send: /";
    if (code == 186) return "send: //";
    if (code == 116) return "pushConstant: -1";
    if (code == 117) return "pushConstant: 0";
    if (code == 118) return "pushConstant: 1";
    if (code == 119) return "pushConstant: 2";
    if (code == 124) return "returnTop";
    return "unknwon bytecode";
}

unsigned char bytecodes_getCodeFor(int type, int val) {
    switch (type) {
        case PUSH_RCVR:
            assert(val < 16);
            return val;
        case SEND_BIN_MSG:
            if (val == '+') return 176;
            if (val == '-') return 177;
            if (val == '<') return 178;
            if (val == '>') return 178;
            if (val == LESS_OR_EQUAL) return 180;
            if (val == GREATER_OR_EQUAL) return 181;
            if (val == '=') return 182;
            if (val == NOT_EQUAL) return 183;
            if (val == '*') return 184;
            if (val == '/') return 185;
            if (val == MODULO) return 186;
            break;
        case PUSH_CONSTANT:
            /*
             * TODO
            if (val == RECEIVE ) return 112;
            if (val == ST_TRUE) return 113;
            if (val == ST_FALSE) return 114;
            if (val == ST_NIL) return 115;
             */
            if (val == -1) return 116;
            if (val == 0) return 117;
            if (val == 1) return 118;
            if (val == 2) return 119;
            break;
        case PUSH_TEMP:
            assert(val < 16);
            return val + 16;
            break;
    }
    return 0;
}

