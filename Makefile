
.PHONY: help
help:
	@echo "Usage: make <target>"
	@echo
	@echo "Targets:"
	@echo "  help       -- show this help"
	@echo "  spec       -- build the specification technical-spec/midgard.pdf"
	@echo "  spec-clean -- clean the latexmk files in technical-spec"

.PHONY: spec
spec:
	$(MAKE) -C technical-spec nix-build

.PHONY: spec-clean
spec-clean:
	$(MAKE) -C technical-spec nix-clean

