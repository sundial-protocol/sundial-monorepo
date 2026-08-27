/**
 * A minimal Lucid Evolution `Provider` backed by sundial-node's HTTP API,
 * so the CLI can build/sign/submit transactions against a live node instead
 * of Lucid's offline Emulator (which is all the tx-generator load-test
 * tooling uses). Same design as demo/midgard-sdk/examples/send-payment.ts —
 * see internal-docs/api.md § 2 for the full rationale.
 */
import {
  Address,
  CML,
  Credential,
  PROTOCOL_PARAMETERS_DEFAULT,
  ProtocolParameters,
  Provider,
  Transaction,
  TxHash,
  UTxO,
} from '@lucid-evolution/lucid';
import { MidgardNodeClient } from '@midgard-manager/tx-generator';

// The fee parameters sundial-node's mempool validation actually checks a
// submitted transaction against (demo/midgard-node/src/database/mempool.ts,
// defaultPhaseAConfig) — build with anything else and the node may reject
// the transaction as underpaying its fee.
const NODE_PROTOCOL_PARAMETERS: ProtocolParameters = {
  ...PROTOCOL_PARAMETERS_DEFAULT,
  minFeeA: 44,
  minFeeB: 155381,
};

export class MidgardNodeProvider implements Provider {
  private readonly client: MidgardNodeClient;

  constructor(baseUrl: string) {
    this.client = new MidgardNodeClient({ baseUrl, enableLogs: false });
  }

  async getProtocolParameters(): Promise<ProtocolParameters> {
    return NODE_PROTOCOL_PARAMETERS;
  }

  async getUtxos(addressOrCredential: Address | Credential): Promise<UTxO[]> {
    if (typeof addressOrCredential !== 'string') {
      throw new Error('MidgardNodeProvider.getUtxos only supports querying by address');
    }
    return this.client.getUtxos(addressOrCredential);
  }

  async submitTx(tx: Transaction): Promise<TxHash> {
    const result = await this.client.submitTransaction(tx);
    if (result.status !== 'SUBMITTED') {
      throw new Error(result.error ?? result.message ?? `Submit failed: ${result.status}`);
    }
    // MidgardNodeClient's SubmitTransactionResult.txId isn't reliable — the
    // node's response never actually sets that field (see api.md § 3) — so
    // compute the real hash from the CBOR ourselves.
    const body = CML.Transaction.from_cbor_hex(tx).body();
    return CML.hash_transaction(body).to_hex();
  }

  getUtxosWithUnit(): Promise<UTxO[]> {
    throw new Error('MidgardNodeProvider: getUtxosWithUnit is not implemented');
  }
  getUtxoByUnit(): Promise<UTxO> {
    throw new Error('MidgardNodeProvider: getUtxoByUnit is not implemented');
  }
  getUtxosByOutRef(): Promise<UTxO[]> {
    throw new Error('MidgardNodeProvider: getUtxosByOutRef is not implemented');
  }
  getDelegation(): Promise<never> {
    throw new Error('MidgardNodeProvider: getDelegation is not implemented');
  }
  getDatum(): Promise<never> {
    throw new Error('MidgardNodeProvider: getDatum is not implemented');
  }
  awaitTx(): Promise<boolean> {
    throw new Error('MidgardNodeProvider: awaitTx is not implemented — use `midgard tx-lookup`');
  }
  evaluateTx(): Promise<never> {
    throw new Error('MidgardNodeProvider: evaluateTx is not implemented');
  }
}
