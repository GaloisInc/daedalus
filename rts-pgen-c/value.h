#ifndef VALUE_H
#define VALUE_H

#include <stdio.h>

typedef enum {
    VInt,
    VDict,
    VList,
    VArray
} ValueType;

typedef struct ValueDict {
    char * key;
    struct Value * value;
    struct ValueDict * next;
} ValueDict;

typedef struct ValueList {
    int len;
    struct Value * value;
    struct ValueList * next;
} ValueList;

typedef struct Value {
    ValueType tag;
    union {
        int intValue;
        ValueDict * dictValue;
        ValueList* listValue;
    };
} Value;


ValueDict * createValueDict();
void printValueDict(ValueDict *);
ValueDict * addDictEntry(char *key, Value *v, ValueDict * src);
Value * getDict(char * key, ValueDict * v);

ValueList* createValueList();
void printValueList(ValueList *);
ValueList* pushListEntry(Value* v, ValueList* src);
ValueList* reverseList(ValueList* src);

Value * createIntValue(int i);
Value * createDictValue(ValueDict* d);
Value * createListValue(ValueList* l);
void printValue(Value *);


#endif /* VALUE_H */
