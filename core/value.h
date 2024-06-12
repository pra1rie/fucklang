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

typedef struct value_t {
    Type type;
    union {
        void *as_ptr;
        double as_num;
        String as_str;
        ValueArray as_arr;
        void *as_obj;
    } value;
} Value;

extern bool equals(Value a, Value b);
extern bool isTrue(Value a);
extern void die();
extern Value objGetField(void *obj, char *field);
extern void objSetField(void *obj, char *field, Value value);

#endif
