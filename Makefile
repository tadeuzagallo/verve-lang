CC = clang++
CFLAGS = -g -O0 -Wall -Wextra -std=c++11 -I .
LIBS = 

CPUS ?= $(shell sysctl -n hw.ncpu || echo 1)
MAKEFLAGS += --jobs=$(CPUS)

define source_glob
$(shell find . -name $(1) -not -path './tests/*')
endef

HEADERS = $(call source_glob, '*.h')
SOURCES = $(call source_glob, '*.cc') $(call source_glob, '*.S')
OBJECTS = $(patsubst %,.build/%.o,$(SOURCES))
TARGET = verve

.PRECIOUS: default $(TARGET) $(OBJECTS)

default: $(TARGET)

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
	$(TEST_RESULTS)

.build/tests/errors/%.test: tests/errors/%.vrv tests/errors/%.err $(TARGET) test_setup 
	$(COUNT_TEST)
	@mkdir -p $$(dirname $@)
	@sh -c "trap '' 6; ./$(TARGET) $<" > /dev/null 2> $@_; \
	if [[ $$? == 0 ]]; then \
		$(TEST_ERROR) \
	else \
		diff \
			-I "libc++abi.dylib: terminating" \
			-I "Abort trap: 6" \
			$@_ $(word 2, $^) && $(TEST_SUCCESS) || $(TEST_FAILURE); \
	fi

# CPP_TESTS

CPP_TESTS = $(patsubst %.cc,.build/%.test,$(wildcard tests/cpp/*.cc))

.PHONY: cpp_tests .build/tests/cpp/%.test
cpp_tests: $(CPP_TESTS) $(OBJECTS) $(HEADERS)
	$(TEST_RESULTS)

.build/tests/cpp/%.test: tests/cpp/%.cc $(OBJECTS) $(HEADERS) test_setup
	$(COUNT_TEST)
	@mkdir -p $$(dirname $@)
	@$(CC) $(CFLAGS) $< $(filter-out %verve.cc.o,$(OBJECTS)) $(LIBS) -I ./ -o $@_
	@$@_; \
	if [[ $$? != 0 ]]; then $(TEST_FAILURE); else $(TEST_SUCCESS); fi

# OUTPUT TESTS

OUTPUT_TESTS = $(patsubst %.vrv,.build/%.test,$(wildcard tests/*.vrv))

.PHONY: output_tests .build/tests/%.test
output_tests: $(OUTPUT_TESTS)
	$(TEST_RESULTS)

.build/tests/%.test: tests/%.vrv tests/%.out $(TARGET) test_setup
	$(COUNT_TEST)
	@mkdir -p $$(dirname $@)
	-@./$(TARGET) $< > $@_; \
	if [[ $$? != 0 ]]; then $(TEST_ERROR); else diff $@_ $(word 2, $^) && $(TEST_SUCCESS) || $(TEST_FAILURE); fi

.PHONY: tests/%.test
tests/%.test: .build/tests/%.test
	@#

# ALL TESTS

.PHONY: test
test: lock_test_results output_tests error_tests cpp_tests
	@rm -f $(TEST_LOCK_FILE)
	$(TEST_RESULTS)

# test support

COUNT_TEST_FILE = .build/test_count
COUNT_FAILURE_FILE = .build/test_failures
TEST_LOCK_FILE = .build/test_lock

TEST_NAME = $(patsubst .build/%,%,$@)
TEST_ERROR = (echo "$(TEST_NAME): ‼️" && $(COUNT_FAILURE))
TEST_FAILURE = (echo "$(TEST_NAME): ❌" && $(COUNT_FAILURE))
TEST_SUCCESS = echo "$(TEST_NAME): ✅"

COUNT_TEST = @$$(touch "$(COUNT_TEST_FILE)/$$(echo $@ | tr -s '/.' '_')")
COUNT_FAILURE = @$$(touch "$(COUNT_FAILURE_FILE)/$$(echo $@ | tr -s '/.' '_')")

TEST_RESULTS = @ \
	if [[ ! -f $(TEST_LOCK_FILE) ]]; then \
		total=$$(ls $(COUNT_TEST_FILE) | wc -l | tr -d ' '); \
		fail=$$(ls $(COUNT_FAILURE_FILE) | wc -l | tr -d ' '); \
		if [[ $$fail != 0 ]]; then \
			echo "❌  $$fail of $$total tests failed"; \
			exit 1; \
		else \
			echo "✅  $$total tests completed"; \
		fi \
	fi

lock_test_results:
	@mkdir -p $$(dirname $(TEST_LOCK_FILE))
	@touch $(TEST_LOCK_FILE)

test_setup:
	@rm -rf $(COUNT_TEST_FILE) $(COUNT_FAILURE_FILE)
	@mkdir -p $(COUNT_TEST_FILE) $(COUNT_FAILURE_FILE)

# INSTALL

install: $(TARGET)
	ln -fs $(PWD)/$(TARGET) /usr/local/bin

install_vim_highlight: $(TARGET)
	mkdir -p ~/.vim/ftdetect ~/.vim/syntax
	ln -fs $(PWD)/vim/ftdetect/verve.vim ~/.vim/ftdetect
	ln -fs $(PWD)/vim/syntax/verve.vim ~/.vim/syntax

# CLEAN

clean:
	-rm -rf $(TARGET) $(TARGET).dSYM .build

.PHONY: clean
