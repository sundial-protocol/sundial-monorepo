<p align="center">
  <img width="150px" src="documentation/public/midgard-icon-green.png" align="center" alt="Midgard Logo"/>
  <h1 align="center">Midgard</h1>
  <p align="center">Cardano's first optimistic rollup solution for highly scalable, secure Layer 2 transactions</p>
</p>

# Build the onchain code

Install Aiken and run `aiken build`.

## Build the spec

Run `make spec` and open `technical-spec/midgard.pdf`.

## Documentation Site

To run the documentation site locally:

```
cd docs           
pnpm install
pnpm dev
```

The documentation will be available at http://localhost:3000

## Contributor guidelines

All contributors must enable the project's standardized git hooks:

```
make enable-git-hooks
```
