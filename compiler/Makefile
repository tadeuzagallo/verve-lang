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

TESTS = $(patsubst %.in,%.test,$(wildcard tests/*.in))

.PHONY: test tests/%.test

test: $(TESTS)

tests/%.test: tests/%.in tests/%.out $(TARGET)
	-@./$(TARGET) -d $< > $@_; \
	if [[ $$? != 0 ]]; then echo "$@: ERROR!"; else diff $@_ $(word 2, $^) && echo "$@: OK!" || echo "$@: FAIL!"; fi

# RUN TESTS

RUN_TESTS = $(patsubst %.ceos,%.test,$(wildcard run_tests/*.ceos))

.PHONY: run_test run_tests/%.test

run_test: $(RUN_TESTS)

run_tests/%.test: run_tests/%.ceos run_tests/%.out $(TARGET)
	-@./$(TARGET) $< > $@_; \
	if [[ $$? != 0 ]]; then echo "$@: ERROR!"; else diff $@_ $(word 2, $^) && echo "$@: OK!" || echo "$@: FAIL!"; fi

# CLEAN

clean:
	-rm -rf $(OBJECTS) $(TARGET) $(TARGET).dSYM $(wildcard tests/*.test_)

.PHONY: clean
