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

ERROR_TESTS = $(patsubst %.vrv,.build/%.test,$(wildcard tests/errors/*.vrv))

.PHONY: error_tests .build/tests/errors/%.test
error_tests: $(ERROR_TESTS)

.build/tests/errors/%.test: tests/errors/%.vrv tests/errors/%.err $(TARGET)
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

.PHONY: cpp_tests .build/tests/cpp/%.test
cpp_tests: $(CPP_TESTS) $(OBJECTS) $(HEADERS)

.build/tests/cpp/%.test: tests/cpp/%.cc $(OBJECTS) $(HEADERS)
	@mkdir -p $$(dirname $@)
	@$(CC) $(CFLAGS) $< $(filter-out %verve.cc.o,$(OBJECTS)) $(LIBS) -I ./ -o $@_
	@$@_; \
	if [[ $$? != 0 ]]; then echo "$@: $(FAILURE)"; else echo "$@: $(SUCCESS)"; fi

# TESTS

TESTS = $(patsubst %.vrv,.build/%.test,$(wildcard tests/*.vrv))

.PHONY: output_tests .build/tests/%.test
output_tests: $(TESTS)

.build/tests/%.test: tests/%.vrv tests/%.out $(TARGET)
	@mkdir -p $$(dirname $@)
	-@./$(TARGET) $< > $@_; \
	if [[ $$? != 0 ]]; then echo "$@: $(ERROR)"; else diff $@_ $(word 2, $^) && echo "$@: $(SUCCESS)" || echo "$@: $(FAILURE)"; fi

.PHONY: tests/%.test
tests/%.test: .build/tests/%.test
	@#


.PHONY: test
test: output_tests error_tests cpp_tests

# CLEAN

install: $(TARGET)
	ln -fs $(PWD)/$(TARGET) /usr/local/bin

clean:
	-rm -rf $(OBJECTS) $(TARGET) $(TARGET).dSYM $(wildcard tests/*.test_)

.PHONY: clean
