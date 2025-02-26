import { clearNodeDatabase, configureNodeEndpoint } from './actions/node.js';
import { configureTxGenerator, toggleTxGenerator } from './actions/tx-generator.js';
import {
  addWalletAction,
  listWalletsAction,
  removeWalletAction,
  walletDetailsAction,
} from './actions/wallet.js';
import type { Menu } from './types.js';

export const menu: Menu = {
  sections: [
    {
      name: 'Node Operations',
      description: 'Configure endpoint and manage node mempool',
      actions: [configureNodeEndpoint, clearNodeDatabase],
    },
    {
      name: 'Wallet Management',
      description: 'Add, remove, and view wallets for transaction signing',
      actions: [addWalletAction, listWalletsAction, walletDetailsAction, removeWalletAction],
    },
    {
      name: 'Transaction Generator',
      description: 'Manage transaction generator settings',
      actions: [configureTxGenerator, toggleTxGenerator],
    },
  ],
};
