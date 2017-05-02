.PHONY: scheme
scheme:
	scheme --libdirs .:nanopass-framework-scheme:lalr-scm

.PHONY: test
test:
	./run-tests.sh

.PHONY: clean
clean:
	rm -f silly_* *.cmx *.cmi *.o
