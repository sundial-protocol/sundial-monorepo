
.PHONY: help
help:
	@echo "Usage: make <target>"
	@echo
	@echo "Targets:"
	@echo "  help      -- show this help"
	@echo "  shell     -- open nix dev shell"
	@echo "  build     -- generate the whitepaper pdf"
	@echo "  nix-build -- open nix shell & build"
	@echo "  clean     -- clean up latexmk files"

.PHONY: shell
shell:
	nix develop

.PHONY: build
build:
	latexmk -pdf -shell-escape -verbose -file-line-error -synctex=1 -interaction=nonstopmode

.PHONY: nix-build
nix-build:
	nix develop --command bash -c "latexmk -pdf -shell-escape -verbose -file-line-error -synctex=1 -interaction=nonstopmode"

.PHONY: clean
clean:
	latexmk -C
