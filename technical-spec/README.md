# Midgard Technical specification

## Building this latex project

Please ensure that nix is installed in your system and flakes are enabled.

1. `cd technical-spec`
2. Opening a nix shell with the necessary latex packages installed: `make shell`
3. Open vscode: `code .`

OR

1. Building the whitepaper in a single step: `pushd technical-spec; make nix-build; popd`
