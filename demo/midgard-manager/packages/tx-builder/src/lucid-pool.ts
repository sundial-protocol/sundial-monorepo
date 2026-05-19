import {
  Emulator,
  EmulatorAccount,
  Lucid,
  LucidEvolution,
  Network,
  PROTOCOL_PARAMETERS_DEFAULT,
  UTxO,
} from '@lucid-evolution/lucid';

// Pool of pre-initialized LucidEvolution instances.
//
// Constructing Lucid(emulator, network, ...) involves async WASM setup that
// costs ~100–300 ms per call. At high TPS, creating one instance per task
// would dominate batch time. This pool pre-initializes `size` instances at
// generator startup and recycles them across tasks so every acquire() is
// effectively instantaneous.

const ZERO_FEE_PARAMS = {
  ...PROTOCOL_PARAMETERS_DEFAULT,
  minFeeA: 0,
  minFeeB: 0,
  priceMem: 0,
  priceStep: 0,
  coinsPerUtxoByte: 0n,
} as const;

async function initInstance(
  walletPrivateKey: string,
  address: string,
  assets: UTxO['assets'],
  network: Network
): Promise<LucidEvolution> {
  const account: EmulatorAccount = {
    seedPhrase: '',
    address,
    assets,
    privateKey: walletPrivateKey,
  };
  const emulator = new Emulator([account]);
  return Lucid(emulator, network, { presetProtocolParameters: ZERO_FEE_PARAMS });
}

export class LucidPool {
  private readonly available: LucidEvolution[];
  private readonly waiters: Array<(lucid: LucidEvolution) => void>;

  private constructor(instances: LucidEvolution[]) {
    this.available = [...instances];
    this.waiters = [];
  }

  static async create(
    size: number,
    walletPrivateKey: string,
    address: string,
    assets: UTxO['assets'],
    network: Network
  ): Promise<LucidPool> {
    const instances = await Promise.all(
      Array.from({ length: size }, () => initInstance(walletPrivateKey, address, assets, network))
    );
    return new LucidPool(instances);
  }

  acquire(): Promise<LucidEvolution> {
    const instance = this.available.pop();
    if (instance !== undefined) {
      return Promise.resolve(instance);
    }
    return new Promise((resolve) => this.waiters.push(resolve));
  }

  release(lucid: LucidEvolution): void {
    const waiter = this.waiters.shift();
    if (waiter !== undefined) {
      waiter(lucid);
    } else {
      this.available.push(lucid);
    }
  }
}
