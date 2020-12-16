#include <stdlib.h>
#include <string.h>
#include "value.h"


void printDictValue(DictValue * kv){

    if (kv == NULL)
        return;
    else {
        printf("k:%s ", kv->key);
        printf("v:");
        printValue(kv->value);
        if (kv->next != NULL) {
            printf(" ");
            printDictValue(kv->next);
        }
    }
}

void printValue(Value * v){

    if (v == NULL) {
        printf("VNULL");
        return;
    }
    switch (v->tag) {
    case VInt: {
        printf("%d", v->intValue);
        break;
    }
    case VDict: {
        printf("{ ");
        printDictValue(v->dictValue);
        printf(" }");
        break;
    }
    default: {
        printf("not handled case printValue\n");
        exit(1);
    }
    }
}

Value * createDictValue(){
    Value * newV = (Value *)malloc(sizeof(Value));

    newV->tag = VDict;
    newV->dictValue = NULL;

    return newV;
}

Value * addDictEntry(char *key, Value *v, Value * src){
    Value * newV = (Value *)malloc(sizeof(Value));

    DictValue * dict = malloc(sizeof(DictValue));

    newV->tag = VDict;
    dict->key = key;
    dict->value = v;
    dict->next = src->dictValue;
    newV->dictValue = dict;

    return newV;
}

Value * createIntValue(int i){
    Value * newV = (Value *)malloc(sizeof(Value));

    newV->tag = VInt;
    newV->intValue = i;

    return newV;
}

Value * getDict(char * key, Value * v) {
    if (v->tag != VDict)
        exit(1);

    DictValue * dict = v->dictValue;
    while (dict != NULL) {
        if (strcmp(dict->key, key) == 0)
            return dict->value;
        dict = dict->next;
    }

    return NULL;
}
