CC=gcc
FLAGS = -Wall -Werror -DDEBUG
SOURCES=$(wildcard src/*.c)
OBJECTS=$(patsubst %.c, %.o, $(SOURCES))


all: Main.out

Main.out: Main.o $(OBJECTS)
	$(CC) $(FLAGS) Main.o $(OBJECTS) -o Main.out

Main.o: Main.c
	$(CC) $(FLAGS) -c Main.c

$(OBJECTS): src/%.o : src/%.c
	$(CC) $(FLAGS) -c $< -o $@

	
clean:
	rm *.o Main.out $(OBJECTS)
