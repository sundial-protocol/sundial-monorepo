
.PHONY: help
help:
	@echo "Usage: make <target>"
	@echo
	@echo "Targets:"
	@echo "  help      -- show this help"
	@echo "  shell     -- open nix dev shell"
	@echo "  build     -- build the specification midgard.pdf"
	@echo "  clean     -- clean up latexmk files"
	@echo "  nix-build -- open nix shell & build"
	@echo "  nix-clean -- open nix shell & clean"

.PHONY: shell
shell:
	nix develop

.PHONY: build
build:
	latexmk -pdf -verbose -file-line-error -synctex=1 -interaction=nonstopmode

.PHONY: clean
clean:
	latexmk -C
	rm -rf svg-inkscape/
	find . -type f -name "*.aux" -exec rm {} \;
	rm gitHeadLocal.gin

.PHONY: nix-build
nix-build:
	nix develop --command $(MAKE) build

.PHONY: nix-clean
nix-clean:
	nix develop --command $(MAKE) clean

