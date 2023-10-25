# generate bytecodes.c
#


BEGIN {
    FS=":";
    getCodeType="";
    getCodeAssert="";
    getCodeReturns="";
}
/^#/ { next };
/^DEF_BEGIN/ {
    getCodeType="";
    getCodeAssert="";
    getCodeReturns="";
    next;
}
/^type:/ {
    getCodeType=$2; next;
}
/^assert:/ {
    getCodeAssert=$2; next;
}
/^returns:/ {
    getCodeReturns=$2; next;
}

/^DEF_END/ {
    printf("case %s:\n", getCodeType);
    printf("  assert(%s);\n", getCodeAssert);
    printf("  return %s;\n", getCodeReturns);
    next;
}

{
    print $0;
}
