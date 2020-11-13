#include <stdlib.h>
#include <string.h>
#include "value.h"


void print_DictValue(DictValue * kv){

    if (kv == NULL)
        return;
    else {
        printf("k:%s ", kv->key);
        printf("v:");
        print_Value(kv->value);
        print_DictValue(kv->next);
    }
}

void print_Value(Value * v){

    if (v == NULL) {
        printf("VNULL\n");
        return;
    }
    switch (v->tag) {
    case VINT: {
        printf("%d ", v->intv);
        break;
    }
    case VDICT: {
        printf("{ ");
        print_DictValue(v->dict);
        printf(" } ");
        break;
    }
    default: {
        printf("not handled case print_value\n");
        exit(1);
    }
    }
}

Value * empty_dict(){
    Value * newV = (Value *)malloc(sizeof(Value));

    newV->tag = VDICT;
    newV->dict = NULL;

    return newV;
}

Value * add_entry_dict(char *key, Value *v, Value * src){
    Value * newV = (Value *)malloc(sizeof(Value));

    DictValue * dict = malloc(sizeof(DictValue));

    newV->tag = VDICT;
    dict->key = key;
    dict->value = v;
    dict->next = src->dict;
    newV->dict = dict;

    return newV;
}

Value * create_value(int i){
    Value * newV = (Value *)malloc(sizeof(Value));

    newV->tag = VINT;
    newV->intv = i;

    return newV;
}

Value * get_dict(char * key, Value * v) {
    if (v->tag != VDICT)
        exit(1);

    DictValue * dict = v->dict;
    while (dict != NULL) {
        if (strcmp(dict->key, key) == 0)
            return dict->value;
        dict = dict->next;
    }

    return NULL;
}
