import { Effect, Either, Option } from "effect";
import {
  Address,
  Assets,
  Credential,
  LucidEvolution,
  PolicyId,
  UTxO,
  fromHex,
  fromUnit,
  toHex,
} from "@lucid-evolution/lucid";
import { blake2b } from "@noble/hashes/blake2b";

export const isHexString = (str: string): boolean => {
  const hexRegex = /^[0-9A-Fa-f]+$/;
  return hexRegex.test(str);
};

export const errorToString = (error: any): string => {
  return error.message ?? JSON.stringify(error);
};

export const getSingleAssetApartFromAda = (
  assets: Assets
): Either.Either<[PolicyId, string, bigint], string> => {
  const flattenedAssets: [string, bigint][] = Object.entries(assets);
  const woLovelace: [string, bigint][] = flattenedAssets.filter(
    ([unit, _qty]) => !(unit === "" || unit === "lovelace")
  );
  if (woLovelace.length === 1) {
    const explodedUnit = fromUnit(woLovelace[0][0]);
    return Either.right([
      explodedUnit.policyId,
      explodedUnit.assetName ?? "",
      woLovelace[0][1],
    ]);
  } else {
    return Either.left("Operation failed");
  }
};

export const utxosAtByNFTPolicyId = (
  lucid: LucidEvolution,
  addressOrCred: Address | Credential,
  policyId: PolicyId
): Effect.Effect<UTxO[], string> =>
  Effect.gen(function* () {
    const allUTxOs = yield* Effect.tryPromise({
      try: () => lucid.utxosAt(addressOrCred),
      catch: (_) => "Failed to fetch state queue UTxOs",
    });
    const nftEithers: Either.Either<UTxO, string>[] = allUTxOs.map(
      (u: UTxO) => {
        const nfts = getSingleAssetApartFromAda(u.assets);
        return Either.andThen(
          nfts,
          ([sym, _tn, qty]): Either.Either<UTxO, string> => {
            if (sym === policyId && qty === 1n) {
              return Either.right(u);
            } else {
              return Either.left("UTxO without expected NFT collection");
            }
          }
        );
      }
    );
    const authenticUTxOs = yield* Effect.filterMap(nftEithers, Option.some);
    return authenticUTxOs;
  });

const blake2bHelper = (
  hash: string,
  dkLen: number,
  functionName: string
): Either.Either<string, string> => {
  if (isHexString(hash)) {
    try {
      return Either.right(toHex(blake2b(fromHex(hash), { dkLen })));
    } catch (e) {
      return Either.left(errorToString(e));
    }
  } else {
    return Either.left(`Invalid hash provided for ${functionName} function`);
  }
};

export const hashHexWithBlake2b224 = (
  hash: string
): Either.Either<string, string> => blake2bHelper(hash, 28, "Blake2b224");

export const hashHexWithBlake2b256 = (
  hash: string
): Either.Either<string, string> => blake2bHelper(hash, 32, "Blake2b256");
