#include <stdio.h>
#include <stdlib.h>
#include "value.h"

extern Value
fuck_print(int argc, Value *argv)
{
    for (int i = 0; i < argc; ++i) {
        printf("%s", core_cstring(argv[i]));
    }
    return (Value){.type = TYPE_NUMBER, .value.as_num = 0};
}

extern Value
fuck_println(int argc, Value *argv)
{
    fuck_print(argc, argv);
    printf("\n");
    return (Value){.type = TYPE_NUMBER, .value.as_num = 0};
}
