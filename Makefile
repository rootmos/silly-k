.PHONY: scheme
scheme:
	scheme --libdirs .:nanopass-framework-scheme:lalr-scm

.PHONY: test
test:
	scheme --libdirs .:nanopass-framework-scheme:lalr-scm --compile-imported-libraries --optimize-level 3 --program tests.scm

.PHONY: clean
clean:
	rm -vf _build

.PHONY: snippet
snippet:
	scheme --libdirs .:nanopass-framework-scheme:lalr-scm --script snippet.scm
