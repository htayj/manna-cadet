.DEFAULT_GOAL := help

export CL_SOURCE_REGISTRY := $(CURDIR)/lisp//:$(CL_SOURCE_REGISTRY)

MODEL ?= model/manna-cadet.lisp

.PHONY: help test check normalize inspect

help:
	@echo "manna-cadet - abstract keyboard-layout frontend"
	@echo ""
	@echo "Targets:"
	@echo "  help   show this message"
	@echo "  test   run the self-test harness"
	@echo "  check  validate MODEL (default: model/manna-cadet.lisp)"
	@echo "  normalize  write canonical MODEL DSL to stdout"
	@echo "  inspect  write abstract MODEL inspection to stdout"

test:
	sbcl --noinform --non-interactive --no-userinit --no-sysinit \
	  --eval "(require :asdf)" \
	  --eval "(asdf:test-system :manna-cadet)" \
	  --quit

check:
	@./bin/manna-cadet check "$(MODEL)"

normalize:
	@./bin/manna-cadet normalize "$(MODEL)"

inspect:
	@./bin/manna-cadet inspect "$(MODEL)"
