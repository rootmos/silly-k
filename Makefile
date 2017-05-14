.PHONY: repl
repl:
	rlwrap ./repl

.PHONY: scheme
scheme:
	scheme --libdirs .:nanopass-framework-scheme:lalr-scm

RUN_TESTS=scheme --libdirs .:nanopass-framework-scheme:lalr-scm --compile-imported-libraries --optimize-level 3 --program tests.scm
.PHONY: test
test:
	$(RUN_TESTS)

.PHONY: test-quick
test-quick:
	QUICK=true $(RUN_TESTS)

.PHONY: clean
clean:
	rm -vf _build

.PHONY: snippet
snippet:
	scheme --libdirs .:nanopass-framework-scheme:lalr-scm --script snippet.scm
