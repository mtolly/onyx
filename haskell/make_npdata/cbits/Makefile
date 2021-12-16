CC = gcc
CFLAGS =
TARGET = make_npdata
OBJS = make_npdata.o aes.o sha1.o lz.o ec.o utils.o

ifeq ($(DEBUG), 1)
CFLAGS+=-g -O0
else
CFLAGS+=-O2
endif

all: $(TARGET)

$(TARGET): $(OBJS)
	$(CC) $(CFLAGS) -o $@ $(OBJS)

clean:
	$(RM) *.o $(TARGET) *.exe
