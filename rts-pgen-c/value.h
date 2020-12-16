#ifndef VALUE_H
#define VALUE_H

#include <stdio.h>

typedef enum {
    VInt,
    VDict,
    VArray
} ValueType;

typedef struct DictValue {
    char * key;
    struct Value * value;
    struct DictValue * next;
} DictValue;

typedef struct Value {
    ValueType tag;
    union {
        int intValue;
        DictValue * dictValue;
    };
} Value;


Value * createDictValue();
void printDictValue(DictValue *);
Value * addDictEntry(char *key, Value *v, Value * src);
Value * getDict(char * key, Value * v);

Value * createIntValue(int i);

void printValue(Value *);


#endif /* VALUE_H */
