import { Lucid, Blockfrost } from "@lucid-evolution/lucid";

const BLOCKFROST_URL = "https://cardano-preprod.blockfrost.io/api/v0";
const BLOCKFROST_KEY = "preprode0IPdksOzu31iDChG9gAAznZVkDP3yh1";
const MAIN_SEED = "leopard faith second appear south announce dinosaur crop vehicle glue play panda amount camera still possible become merit innocent marine fly vivid ecology produce";
const BC_SEED = "gun senior surface document memory hip comfort lens bid cruise recycle spirit wall pencil ask";

const lucid = await Lucid(new Blockfrost(BLOCKFROST_URL, BLOCKFROST_KEY), "Preprod");

lucid.selectWallet.fromSeed(BC_SEED);
const bcAddress = await lucid.wallet().address();
console.log("Block commitment wallet:", bcAddress);
const bcUtxos = await lucid.wallet().getUtxos();
const bcBalance = bcUtxos.reduce((sum, u) => sum + u.assets.lovelace, 0n);
console.log("Current balance:", Number(bcBalance) / 1_000_000, "ADA");

lucid.selectWallet.fromSeed(MAIN_SEED);
const tx = await lucid.newTx()
  .pay.ToAddress(bcAddress, { lovelace: 1_000_000_000n })
  .complete();
const signed = await tx.sign.withWallet().complete();
const txHash = await signed.submit();
console.log("Tx submitted:", txHash);
console.log("1000 ADA sent to block commitment wallet.");
