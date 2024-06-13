#include <stdio.h>
#include <stdlib.h>
#include "value.h"

extern char *core_cstring(Value arg);

extern Value
print(int argc, Value *argv)
{
    for (int i = 0; i < argc; ++i) {
        printf("%s", core_cstring(argv[i]));
    }
    return (Value){.type = TYPE_NUMBER, .value.as_num = 0};
}

extern Value
println(int argc, Value *argv)
{
    print(argc, argv);
    printf("\n");
    return (Value){.type = TYPE_NUMBER, .value.as_num = 0};
}
