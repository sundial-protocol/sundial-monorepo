# Sundial Monorepo
A collection of tools and libraries comprising the Sundial Layer 2 blockchain.
Submodules are listed below.

# Documentation
`/internal-docs` - Internal documentation for Sundial developers.

`/indexer-openapi` - OpenAPI specification for Sundial Indexer API.

`/indexer-utxorpc` - UTXO RPC specification for Sundial Indexer.

`/technical-spec` - Technical specification documents for the Sundial/Midgard L2.

`/documentation` - User and developer documentation site for the Sundial/Midgard L2.

# Staking & Defi
`/ada-locker` - ADA smart contracts for Sundial Staking, written in Aiken & Typescript.

`/scalus-locker` - Ada smart contracts for Sundial Staking & Lending, written in Scalus & Typescript.

`/btc-locker` - BTC Scripts for Sundial Staking.

`/airdrop-engine` - Airdrop engine for distributing yield to Sundial users.

# Bridging
`/charms-cardano` - Cardano side of the Charms bridge, written in Aiken by Sundial developers.

# Layer 2

`/onchain` - Onchain code for the Sundial Layer 2 blockchain, written in Aiken & Plutarch.

`/demo` - Demo Layer 2 implementation for testing and prototyping.

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
