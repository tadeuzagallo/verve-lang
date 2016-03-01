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

# TESTS

TESTS = $(patsubst %.in,%.test,$(wildcard tests/*.in))

.PHONY: test %.test

test: $(TARGET) $(TESTS)

%.test: %.in %.out
	-@./$(TARGET) $< $@_; \
	if [[ $$? != 0 ]]; then echo "$@: ERROR!"; else diff $@_ $(word 2, $^) && echo "$@: OK!" || echo "$@: FAIL!"; fi

# CLEAN

clean:
	-rm -rf $(OBJECTS) $(TARGET) $(TARGET).dSYM $(wildcard tests/*.test_)

.PHONY: clean
