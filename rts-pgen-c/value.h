#ifndef VALUE_H
#define VALUE_H

#include <stdio.h>

#define VINT 0
#define VDICT 1
#define VARRAY 2


typedef struct DictValue {
    char * key;
    struct Value * value;
    struct DictValue * next;
} DictValue;

typedef struct Value {
    int tag;
    union {
        int intv;
        DictValue * dict;
    };
} Value;

void print_DictValue(DictValue *);
void print_Value(Value *);
Value * empty_dict();
Value * add_entry_dict(char *key, Value *v, Value * src);
Value * create_value(int i);
Value * get_dict(char * key, Value * v);

#endif /* VALUE_H */
