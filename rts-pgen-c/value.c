#include <stdlib.h>
#include <string.h>
#include "value.h"
#include "util.h"

void printValueDict(ValueDict * kv){

    if (kv == NULL)
        return;
    else {
        printf("k:%s ", kv->key);
        printf("v:");
        printValue(kv->value);
        if (kv->next != NULL) {
            printf(" ");
            printValueDict(kv->next);
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
        if (v->dictValue != NULL)
            printValueDict(v->dictValue);
        printf(" }");
        break;
    }
    case VList: {
        printf("[ ");
        if (v->listValue != NULL)
            printValueList(v->listValue);
        printf(" ]");
        break;
    }
    default: {
        printf("not handled case printValue\n");
        exit(1);
    }
    }
}

ValueDict * createValueDict(){
    return NULL;
}

ValueDict * addDictEntry(char *key, Value *v, ValueDict * src){
    ValueDict * dict = ALLOCMEM(sizeof(ValueDict));

    dict->key = key;
    dict->value = v;
    dict->next = src;

    return dict;
}

Value * getDict(char * key, ValueDict * d) {
    ValueDict * dict = d;
    while (dict != NULL) {
        if (strcmp(dict->key, key) == 0)
            return dict->value;
        dict = dict->next;
    }

    return NULL;
}

ValueList* createValueList() {
    return NULL;
}

void printValueList(ValueList * list) {
    if (list == NULL || list->value == NULL)
        return;
    else {
        printValue(list->value);
        if (list->next != NULL) {
            printf(", ");
            printValueList(list->next);
        }
    }
}

ValueList* pushListEntry(Value* v, ValueList* src) {
    ValueList * list = ALLOCMEM(sizeof(ValueList));
    list->len = src == NULL ? 1 : src->len + 1;
    list->value = v;
    list->next = src;

    return list;
}

ValueList* reverseList(ValueList* lst) {
    if (lst == NULL)
        return NULL;

    ValueList* last = NULL;
    int len = 0;
    do {
        ValueList* r = ALLOCMEM(sizeof(ValueList));
        r->len = ++len ;
        r->value = lst->value;
        r->next = last;
        last = r;
        lst = lst->next;
    } while(lst != NULL);

    return last;
}


Value * createIntValue(int i){
    Value * newV = (Value *)ALLOCMEM(sizeof(Value));

    newV->tag = VInt;
    newV->intValue = i;

    return newV;
}

Value * createDictValue(ValueDict* d) {
    Value * newV = (Value *)ALLOCMEM(sizeof(Value));

    newV->tag = VDict;
    newV->dictValue = d;

    return newV;
}

Value * createListValue(ValueList* l) {
    Value * newV = (Value *)ALLOCMEM(sizeof(Value));

    newV->tag = VList;
    newV->listValue = l;

    return newV;
}