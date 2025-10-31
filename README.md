# Sundial Monorepo
A collection of tools and libraries comprising the Sundial Layer 2 blockchain.
Submodules are listed below.

# Documentation
`/internal-docs` - Internal documentation for Sundial developers.

`/indexer-openapi` - OpenAPI specification for Sundial Indexer API.

`/indexer-utxorpc` - UTXO RPC specification for Sundial Indexer.

# Staking & Defi
`/ada-locker` - ADA smart contracts for Sundial Staking, written in Aiken & Typescript.

`/scalus-locker` - Ada smart contracts for Sundial Staking & Lending, written in Scalus & Typescript.

`/btc-locker` - BTC Scripts for Sundial Staking.

`/airdrop-engine` - Airdrop engine for distributing yield to Sundial users.

# Layer 2

## Build the onchain code

Install [Aiken](https://aiken-lang.org/) and run 

``` 
aiken build
```

## Technical Specification

Run 
```
make spec
```

 and open `technical-spec/midgard.pdf`.

## Contributor guidelines

All contributors must enable the project's standardized git hooks:

```
make enable-git-hooks
```

Take a look at the [contribution guidelines](./CONTRIBUTING.md) for more details.

## Documentation / Architectural Decision Records (ADRs)

To run the documentation site locally:

1. Navigate to documentation

```
cd documentation
```

2. Install dependencies

```
pnpm install
```

3. Start development server

```
pnpm dev
```

> Local development server will be available at http://localhost:3000, you can read more about how to add new documentation [here](https://github.com/Anastasia-Labs/midgard/blob/main/documentation/README.md).
