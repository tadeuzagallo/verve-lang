CC = clang++
CFLAGS = -g -O0 -Wall -Wextra -std=c++11 -I .
LIBS = 

define source_glob
$(shell find . -name $(1) -not -path './tests/*')
endef

HEADERS = $(call source_glob, '*.h')
SOURCES = $(call source_glob, '*.cc') $(call source_glob, '*.S')
OBJECTS = $(patsubst %,%.o,$(SOURCES))
TARGET = ceos

default: $(TARGET)

.PRECIOUS: $(TARGET) $(OBJECTS)

$(TARGET): $(OBJECTS)
	echo $(SOURCES)
	$(CC) $(CFLAGS) $(OBJECTS) $(LIBS) -o $@

%.cc.o: %.cc $(HEADERS)
	$(CC) $(CFLAGS) -c $< -o $@

%.S.o: %.S $(HEADERS)
	$(CC) $(CFLAGS) -c $< -o $@

# ERROR TESTS

ERROR_TESTS = $(patsubst %.ceos,%.test,$(wildcard tests/errors/*.ceos))

.PHONY: error_test tests/errors/%.test

error_test: $(ERROR_TESTS)

tests/errors/%.test: tests/errors/%.ceos tests/errors/%.err $(TARGET)
	@sh -c "trap '' 6; ./$(TARGET) $<" > /dev/null 2> $@_; \
	if [[ $$? == 0 ]]; then \
		echo "$@: ERROR!"; \
	else \
		diff \
			-I "libc++abi.dylib: terminating" \
			-I "Abort trap: 6" \
			$@_ $(word 2, $^) && echo "$@: OK!" || echo "$@: FAIL!"; \
	fi

CPP_TESTS = $(patsubst %.cc,%.test,$(wildcard tests/cpp/*.cc))

.PHONY: cpp_test tests/cpp/%.test
cpp_test: $(CPP_TESTS) $(OBJECTS) $(HEADERS)

tests/cpp/%.test: tests/cpp/%.cc $(OBJECTS) $(HEADERS)
	@$(CC) $(CFLAGS) $< $(filter-out ./ceos.cc.o,$(OBJECTS)) $(LIBS) -I ./ -o $@_
	@$@_; \
	if [[ $$? != 0 ]]; then echo "$@: FAIL!"; else echo "$@: OK!"; fi

# TESTS

TESTS = $(patsubst %.ceos,%.test,$(wildcard tests/*.ceos))

.PHONY: test tests/%.test

test: $(TESTS) error_test cpp_test

tests/%.test: tests/%.ceos tests/%.out $(TARGET)
	-@./$(TARGET) $< > $@_; \
	if [[ $$? != 0 ]]; then echo "$@: ERROR!"; else diff $@_ $(word 2, $^) && echo "$@: OK!" || echo "$@: FAIL!"; fi

# CLEAN

clean:
	-rm -rf $(OBJECTS) $(TARGET) $(TARGET).dSYM $(wildcard tests/*.test_)

.PHONY: clean
