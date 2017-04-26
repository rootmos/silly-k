.PHONY: scheme
scheme:
	scheme --libdirs nanopass-framework-scheme

.PHONY: test
test:
	./run-tests.sh
