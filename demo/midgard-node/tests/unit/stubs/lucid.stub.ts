export const CML = {
  Transaction: {
    from_cbor_bytes: () => {
      throw new Error(
        "CML.Transaction.from_cbor_bytes is not implemented in unit stubs.",
      );
    },
    new: () => {
      throw new Error("CML.Transaction.new is not implemented in unit stubs.");
    },
  },
  TransactionBody: {
    new: () => {
      throw new Error(
        "CML.TransactionBody.new is not implemented in unit stubs.",
      );
    },
  },
  TransactionWitnessSet: {
    new: () => {
      throw new Error(
        "CML.TransactionWitnessSet.new is not implemented in unit stubs.",
      );
    },
  },
  TransactionInput: {
    new: () => {
      throw new Error(
        "CML.TransactionInput.new is not implemented in unit stubs.",
      );
    },
  },
  TransactionInputList: {
    new: () => {
      throw new Error(
        "CML.TransactionInputList.new is not implemented in unit stubs.",
      );
    },
  },
  TransactionOutputList: {
    new: () => {
      throw new Error(
        "CML.TransactionOutputList.new is not implemented in unit stubs.",
      );
    },
  },
  hash_transaction: () => {
    throw new Error("CML.hash_transaction is not implemented in unit stubs.");
  },
};
