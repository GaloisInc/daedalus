
RTS ?= ../../rts-pgen-c
LOGLEVEL ?= NONE
ODIR = .
ENABLE_ASSERT ?= 1
CFLAGS = -std=c11 -Wall -g -I ${RTS} -D LOGLEVEL=${LOGLEVEL} -DENABLE_ASSERT=${ENABLE_ASSERT}

OBJECTS := $(patsubst ${RTS}/%.c,%.o,$(wildcard ${RTS}/*.c)) grammar.o
vpath %.c ${RTS}
vpath %.h ${RTS}

grammar: ${OBJECTS}

all: grammar

.PHONY : clean
clean:
	rm -f *.o grammar
