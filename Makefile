EMACS ?= emacs
BATCH := $(EMACS) --batch -L .

.PHONY: all compile test clean

all: compile test

compile:
	$(BATCH) -f batch-byte-compile easy-access.el easy-access-tests.el

test:
	$(BATCH) -l easy-access-tests.el -f ert-run-tests-batch-and-exit

clean:
	rm -f *.elc
