#ifndef CORE_VALUE_H
#define CORE_VALUE_H
#include <stdlib.h>
#include <stdbool.h>

#define STR_FMT "%.*s"
#define STR_ARG(str)  str.size, str.data
#define STR_ARGP(str) str->size, str->data

typedef enum value_type_e {
    TYPE_NUMBER,
    TYPE_STRING,
    TYPE_ARRAY,
    TYPE_POINTER,
    TYPE_FUNCTION,
    TYPE_STRUCT,
} Type;

typedef struct string_t {
    size_t size;
    char *data;
} String;

typedef struct value_array_t {
    size_t size;
    struct value_t *data;
} ValueArray;

typedef struct value_function_t {
    void *args;
    void *expr;
    size_t padding; // lmfao
} ValueFunction;

typedef struct value_t {
    Type type;
    union {
        void *as_ptr;
        double as_num;
        String as_str;
        ValueArray as_arr;
        ValueFunction as_fun;
        void *as_obj;
    } value;
} Value;

extern bool equals(Value a, Value b);
extern bool isTrue(Value a);
extern void die(int n);
extern Value callFunction(ValueFunction fun, int argc, Value *argv);
extern Value objGetField(void *obj, char *field);
extern void objSetField(void *obj, char *field, Value value);

extern char* core_ctypeof(Value val);
extern char* core_cstring(Value val);
extern void* core_alloc(size_t sz);
extern void* core_realloc(void *ptr, size_t sz);
extern void core_free(void *ptr);

#endif
