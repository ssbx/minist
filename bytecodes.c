#include "bytecodes.h"
#include "parse.tab.h"
#include <assert.h>

unsigned char bytecodes_getCodeFor(int type, int val) {
    switch (type) {
        case PUSH_RCVR:
            /* 0 - 15 */
            assert(val < 16);
            return val;
        case PUSH_TEMP:
            /* 16 - 31 */
            assert(val < 16);
            return 16 + val;
        case PUSH_LITERAL_CONSTANT:
            /* 32 - 63 TODO */
            break;
        case PUSH_LITERAL_VARIABLE:
            /* 64 - 95 TODO */
            break;
        case POP_STORE_RCVR:
            /* 96 - 103 */
            assert(val < 7);
            return 96 + val;
        case POP_STORE_TEMP:
            /* 104 - 111 */
            assert(val < 7);
            return 104 + val;
        case PUSH_CONSTANT:
            /* 112 - 119 TODO */
            /*
            if (val == RECEIVER ) return 112;
            if (val == ST_TRUE) return 113;
            if (val == ST_FALSE) return 114;
            if (val == ST_NIL) return 115;
             */
            if (val == -1) return 116;
            if (val == 0) return 117;
            if (val == 1) return 118;
            if (val == 2) return 119;
            break;
        case RETURN_CONSTANT:
            /* 120 - 123 TODO */
            break;
        case RETURN_STACK_TOP_FROM:
            /* 124 - 125 TODO */
            break;
        case SEND_BIN_MSG:
            /* 176 - 191 */
            if (val == '+') return 176;
            if (val == '-') return 177;
            if (val == '<') return 178;
            if (val == '>') return 179;
            if (val == LESS_OR_EQUAL) return 180;
            if (val == GREATER_OR_EQUAL) return 181;
            if (val == '=') return 182;
            if (val == NOT_EQUAL) return 183;
            if (val == '*') return 184;
            if (val == '/') return 185;
            if (val == MODULO) return 186;
            /*
            if (val == ) return 187; /makepoint
            if (val == ) return 188; /bitshift
            if (val == ) return 189; /div
            if (val == ) return 190; /bitand
            if (val == ) return 191; /bitor
            */
            break;
        case SEND_LITERAL_NOARG:
            /* 208 - 223 */
            assert(val < 16);
            return 208 + val;
        case SEND_LITERAL_1ARG:
            /* 224 - 239 */
            assert(val < 16);
            return 224 + val;

    }
    return 138; // unused
}


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
    if (code == 16) return "pushTemp: 0";
    if (code == 17) return "pushTemp: 1";
    if (code == 18) return "pushTemp: 2";
    if (code == 19) return "pushTemp: 3";
    if (code == 96) return "popAndStoreRcvr: 0";
    if (code == 97) return "popAndStoreRcvr: 1";
    if (code == 98) return "popAndStoreRcvr: 2";
    if (code == 99) return "popAndStoreRcvr: 3";
    if (code == 104) return "popAndStoreTemp: 0";
    if (code == 105) return "popAndStoreTemp: 1";
    if (code == 106) return "popAndStoreTemp: 2";
    if (code == 107) return "popAndStoreTemp: 3";
    if (code == 108) return "popAndStoreTemp: 4";
    if (code == 109) return "popAndStoreTemp: 5";
    if (code == 116) return "pushConstant: -1";
    if (code == 117) return "pushConstant: 0";
    if (code == 118) return "pushConstant: 1";
    if (code == 119) return "pushConstant: 2";
    if (code == 120) return "returnReceiver";
    if (code == 121) return "returnTrue";
    if (code == 122) return "returnFalse";
    if (code == 123) return "returnNil";
    if (code == 124) return "returnTop";
    if (code == 138) return "!wrong bytecode";
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
    if (code == 208) return "sendLiteralSelectorNoarg: 0";
    if (code == 209) return "sendLiteralSelectorNoarg: 1";
    if (code == 210) return "sendLiteralSelectorNoarg: 2";
    if (code == 211) return "sendLiteralSelectorNoarg: 3";
    if (code == 212) return "sendLiteralSelectorNoarg: 4";
    if (code == 213) return "sendLiteralSelectorNoarg: 5";
    if (code == 214) return "sendLiteralSelectorNoarg: 6";
    if (code == 215) return "sendLiteralSelectorNoarg: 7";
    if (code == 216) return "sendLiteralSelectorNoarg: 8";
    if (code == 217) return "sendLiteralSelectorNoarg: 9";
    if (code == 218) return "sendLiteralSelectorNoarg: 10";
    if (code == 219) return "sendLiteralSelectorNoarg: 11";
    if (code == 220) return "sendLiteralSelectorNoarg: 12";
    if (code == 221) return "sendLiteralSelectorNoarg: 13";
    if (code == 222) return "sendLiteralSelectorNoarg: 14";
    if (code == 223) return "sendLiteralSelectorNoarg: 15";

    if (code == 224) return "sendLiteralSelector1arg: 0";
    if (code == 225) return "sendLiteralSelector1arg: 1";
    if (code == 226) return "sendLiteralSelector1arg: 2";
    if (code == 227) return "sendLiteralSelector1arg: 3";
    if (code == 228) return "sendLiteralSelector1arg: 4";
    if (code == 229) return "sendLiteralSelector1arg: 5";
    if (code == 230) return "sendLiteralSelector1arg: 6";
    if (code == 231) return "sendLiteralSelector1arg: 7";
    if (code == 232) return "sendLiteralSelector1arg: 8";
    if (code == 233) return "sendLiteralSelector1arg: 9";
    if (code == 234) return "sendLiteralSelector1arg: 10";
    if (code == 235) return "sendLiteralSelector1arg: 11";
    if (code == 236) return "sendLiteralSelector1arg: 12";
    if (code == 237) return "sendLiteralSelector1arg: 13";
    if (code == 238) return "sendLiteralSelector1arg: 14";
    if (code == 239) return "sendLiteralSelector1arg: 15";



    return "unknwon bytecode";
}


