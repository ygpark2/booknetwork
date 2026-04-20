.PHONY: help dev run test compile clean

SBT ?= sbt

help:
	@printf "Available targets:\n"
	@printf "  make dev      Start Play dev server with file watching (~run)\n"
	@printf "  make run      Start Play server once (run)\n"
	@printf "  make test     Run the test suite\n"
	@printf "  make compile  Compile the application\n"
	@printf "  make clean    Clean build artifacts\n"

dev:
	$(SBT) "~run"

run:
	$(SBT) run

test:
	$(SBT) test

compile:
	$(SBT) compile

clean:
	$(SBT) clean
