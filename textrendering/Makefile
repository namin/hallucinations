CC = gcc
CXX = g++

FLAGS = -I/usr/local//include/freetype2 -I/usr/local//include
LIBS = -L/usr/local//lib -Wl,--rpath -Wl,/usr/local//lib -lfreetype -lz
LIBS += -lGL -lGLU -lglut

all:: textrender

clean:
	-rm *.o textrender



textrender: textrender.o
	${CXX} textrender.o $(LIBS) -o textrender


%.o: %.c %.h
	$(CC) -c ${FLAGS} $*.c

%.o: %.c
	$(CC) -c ${FLAGS} $<
