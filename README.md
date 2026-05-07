# Sundial Monorepo
A collection of tools and libraries comprising the Sundial Layer 2 blockchain.
Submodules are listed below.

<table>
  <tr>
    <td>
      <!-- badge:build:start -->
<img alt="Build" src="https://img.shields.io/badge/build-passing-brightgreen?style=flat-square" />
<!-- badge:build:end -->
    </td>
    <td>
      <!-- badge:quality:start -->
<img alt="Quality" src="https://img.shields.io/badge/quality-failing-red?style=flat-square" />
<!-- badge:quality:end -->
    </td>
    <td>
      <!-- badge:security:start -->
<img alt="Security" src="https://img.shields.io/badge/security-failing-red?style=flat-square" />
<!-- badge:security:end -->
    </td>
      </tr>
        <tr>
        <td>
      <!-- badge:unit:start -->
<img alt="Unit Tests" src="https://img.shields.io/badge/unit%20tests-failing-red?style=flat-square" />
<!-- badge:unit:end -->
    </td>
    <td>
      <!-- badge:integration:start -->
<img alt="Integration Tests" src="https://img.shields.io/badge/integration%20tests-failing-red?style=flat-square" />
<!-- badge:integration:end -->
    </td>
    <td>
      &nbsp;
    </td>
  </tr>
  <tr>
    <td>
      <!-- badge:node-coverage:start -->
<img alt="Node Coverage" src="https://img.shields.io/badge/node%20coverage-70.5%25-orange?style=flat-square" />
<!-- badge:node-coverage:end -->
    </td>
    <td>
      <!-- badge:sdk-coverage:start -->
<img alt="SDK Coverage" src="https://img.shields.io/badge/sdk%20coverage-75.9%25-orange?style=flat-square" />
<!-- badge:sdk-coverage:end -->
    </td>
    <td>
      <!-- badge:ts-coverage:start -->
<img alt="TS Coverage" src="https://img.shields.io/badge/ts%20coverage-66.1%25-orange?style=flat-square" />
<!-- badge:ts-coverage:end -->
    </td>
  </tr>
</table>

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
