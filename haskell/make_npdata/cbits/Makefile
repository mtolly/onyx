CC = gcc
CFLAGS =

ifeq ($(DEBUG), 1)
CFLAGS+=-g -O0
else
CFLAGS+=-O2
endif

TARGET = make_npdata
OBJS = make_npdata.o aes.o sha1.o lz.o utils.o

all: $(TARGET)

$(TARGET): $(OBJS)
	$(CC) $(CFLAGS) -o $@ $(OBJS)


