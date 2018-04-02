default: build
	@true

test:
	lit -v tests

%:
	stack $@ $(STACK_ARGS)
