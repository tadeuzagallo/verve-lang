CC = clang++
CFLAGS = -g -O0 -Wall -Wextra -std=c++11
LIBS = 

HEADERS = $(wildcard *.h)
SOURCES = $(wildcard *.cpp)
OBJECTS = $(patsubst %.cpp,%.o,$(SOURCES))
TARGET = ceos-build

default: $(TARGET)

.PRECIOUS: $(TARGET) $(OBJECTS)

$(TARGET): $(OBJECTS)
	$(CC) $(CFLAGS) $(OBJECTS) $(LIBS) -o $@

%.o: %.cpp $(HEADERS)
	$(CC) $(CFLAGS) -c $< -o $@

clean:
	-rm -rf $(OBJECTS) $(TARGET) $(TARGET).dSYM
