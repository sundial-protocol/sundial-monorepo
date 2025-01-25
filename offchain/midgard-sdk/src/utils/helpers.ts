import { Either } from "effect";
import { Assets, PolicyId, fromUnit } from "@lucid-evolution/lucid";

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
