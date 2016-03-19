CC = clang++
CFLAGS = -g -O0 -Wall -Wextra -std=c++11
LIBS = 

HEADERS = $(wildcard *.h)
SOURCES = $(wildcard *.cc)
OBJECTS = $(patsubst %.cc,%.o,$(SOURCES))
TARGET = ceos-build

default: $(TARGET)

.PRECIOUS: $(TARGET) $(OBJECTS)

$(TARGET): $(OBJECTS)
	$(CC) $(CFLAGS) $(OBJECTS) $(LIBS) -o $@

%.o: %.cc $(HEADERS)
	$(CC) $(CFLAGS) -c $< -o $@

# TESTS

TESTS = $(patsubst %.ceos,%.test,$(wildcard tests/*.ceos))

.PHONY: test tests/%.test

test: $(TESTS)

tests/%.test: tests/%.ceos tests/%.out $(TARGET)
	-@./$(TARGET) $< > $@_; \
	if [[ $$? != 0 ]]; then echo "$@: ERROR!"; else diff $@_ $(word 2, $^) && echo "$@: OK!" || echo "$@: FAIL!"; fi

# CLEAN

clean:
	-rm -rf $(OBJECTS) $(TARGET) $(TARGET).dSYM $(wildcard tests/*.test_)

.PHONY: clean
