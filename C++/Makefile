CC=g++
FLAGS = -Wall -Werror -DDEBUG
SOURCES=$(wildcard src/*.cpp)
OBJECTS=$(patsubst %.cpp, %.o, $(SOURCES))


all: Main.out

Main.out: Main.o $(OBJECTS)
	$(CC) $(FLAGS) Main.o $(OBJECTS) -o Main.out

Main.o: Main.cpp
	$(CC) $(FLAGS) -c Main.cpp 

$(OBJECTS): src/%.o : src/%.cpp
	$(CC) $(FLAGS) -c $< -o $@

	
clean:
	rm *.o Main.out $(OBJECTS)
