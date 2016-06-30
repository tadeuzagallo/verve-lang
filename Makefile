CC = clang++
CFLAGS = -g -O0 -Wall -Wextra -std=c++14 -I .
LIBS = 

define source_glob
$(shell find . -name $(1) -not -path './tests/*')
endef

SUCCESS = ✅
FAILURE = ❌
ERROR = ‼️

HEADERS = $(call source_glob, '*.h')
SOURCES = $(call source_glob, '*.cc') $(call source_glob, '*.S')
OBJECTS = $(patsubst %,.build/%.o,$(SOURCES))
TARGET = verve

default:
	@make -j $(shell sysctl -n hw.ncpu) $(TARGET)

.PRECIOUS: $(TARGET) $(OBJECTS)

$(TARGET): $(OBJECTS)
	$(CC) $(CFLAGS) $(OBJECTS) $(LIBS) -o $@

.build/%.cc.o: %.cc $(HEADERS)
	@mkdir -p $$(dirname $@)
	$(CC) $(CFLAGS) -c $< -o $@

.build/%.S.o: %.S $(HEADERS)
	@mkdir -p $$(dirname $@)
	$(CC) $(CFLAGS) -c $< -o $@

# ERROR TESTS

ERROR_TESTS = $(patsubst %.v,.build/%.test,$(wildcard tests/errors/*.v))

.PHONY: error_test .build/tests/errors/%.test

error_test: $(ERROR_TESTS)

.build/tests/errors/%.test: tests/errors/%.v tests/errors/%.err $(TARGET)
	@mkdir -p $$(dirname $@)
	@sh -c "trap '' 6; ./$(TARGET) $<" > /dev/null 2> $@_; \
	if [[ $$? == 0 ]]; then \
		echo "$@: $(ERROR)"; \
	else \
		diff \
			-I "libc++abi.dylib: terminating" \
			-I "Abort trap: 6" \
			$@_ $(word 2, $^) && echo "$@: $(SUCCESS)" || echo "$@: $(FAILURE)"; \
	fi

CPP_TESTS = $(patsubst %.cc,.build/%.test,$(wildcard tests/cpp/*.cc))

.PHONY: cpp_test .build/tests/cpp/%.test
cpp_test: $(CPP_TESTS) $(OBJECTS) $(HEADERS)

.build/tests/cpp/%.test: tests/cpp/%.cc $(OBJECTS) $(HEADERS)
	@mkdir -p $$(dirname $@)
	@$(CC) $(CFLAGS) $< $(filter-out %verve.cc.o,$(OBJECTS)) $(LIBS) -I ./ -o $@_
	@$@_; \
	if [[ $$? != 0 ]]; then echo "$@: $(FAILURE)"; else echo "$@: $(SUCCESS)"; fi

# TESTS

TESTS = $(patsubst %.v,.build/%.test,$(wildcard tests/*.v))

.PHONY: test .build/tests/%.test

test: $(TESTS) error_test cpp_test
	@#

.build/tests/%.test: tests/%.v tests/%.out $(TARGET)
	@mkdir -p $$(dirname $@)
	-@./$(TARGET) $< > $@_; \
	if [[ $$? != 0 ]]; then echo "$@: $(ERROR)"; else diff $@_ $(word 2, $^) && echo "$@: $(SUCCESS)" || echo "$@: $(FAILURE)"; fi

.PHONY: tests/%.test

tests/%.test: .build/tests/%.test
	@#

# CLEAN

clean:
	-rm -rf $(OBJECTS) $(TARGET) $(TARGET).dSYM $(wildcard tests/*.test_)

.PHONY: clean
