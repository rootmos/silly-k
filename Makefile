.PHONY: scheme
scheme:
	scheme --libdirs .:nanopass-framework-scheme:lalr-scm

.PHONY: test
test:
	./run-tests.sh

.PHONY: clean
clean:
	rm -vf silly_* tests/*.cmx tests/*.cmi tests/*.o tests/*.bin tests/*.mlf

.PHONY: snippet
snippet:
	scheme --libdirs .:nanopass-framework-scheme:lalr-scm --script snippet.scm
