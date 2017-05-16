SCHEME=scheme --libdirs .:nanopass-framework-scheme:lalr-scm --compile-imported-libraries --optimize-level 3

.PHONY: repl
repl:
	$(SCHEME) --script repl.scm

.PHONY: scheme
scheme:
	$(SCHEME)

.PHONY: precompile
precompile:
	echo "(import (silly-k))" | $(SCHEME)

RUN_TESTS=$(SCHEME) --program tests.scm
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
	$(SCHEME) --script snippet.scm
