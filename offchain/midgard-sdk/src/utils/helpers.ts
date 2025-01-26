import { Effect, Either, Option } from "effect";
import {
  Address,
  Assets,
  Credential,
  LucidEvolution,
  PolicyId,
  UTxO,
  fromUnit,
} from "@lucid-evolution/lucid";

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
    const nftEithers: Either.Either<UTxO, string>[] =
      allUTxOs.map((u: UTxO) => {
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
      });
    const authenticUTxOs = yield* Effect.filterMap(
      nftEithers,
      Option.some
    );
    return authenticUTxOs;
  });
