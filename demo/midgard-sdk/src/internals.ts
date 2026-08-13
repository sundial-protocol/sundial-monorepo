import { Effect } from "effect";
import {
  Address,
  Assets as LucidAssets,
  Data,
  fromUnit,
  LucidEvolution,
  PolicyId,
  UTxO,
} from "@lucid-evolution/lucid";
import {
  AssetError,
  DataCoercionError,
  LucidError,
  UnauthenticUtxoError,
} from "@/errors.js";

const getSingleAssetApartFromAda = (
  assets: LucidAssets,
): Effect.Effect<[PolicyId, string, bigint], AssetError> =>
  Effect.gen(function* () {
    const flattenedAssets: [string, bigint][] = Object.entries(assets);
    const woLovelace: [string, bigint][] = flattenedAssets.filter(
      ([unit, _qty]) => !(unit === "" || unit === "lovelace"),
    );
    if (woLovelace.length === 1) {
      const explodedUnit = fromUnit(woLovelace[0][0]);
      return [
        explodedUnit.policyId,
        explodedUnit.assetName ?? "",
        woLovelace[0][1],
      ];
    }

    return yield* Effect.fail(
      new AssetError({
        message: "Failed to get single asset apart from ADA",
        cause: "Expected exactly 1 additional asset apart from ADA",
      }),
    );
  });

/**
 * Similar to `getSingleAssetApartFromAda`, with the additional requirement for
 * the quantity to be exactly 1.
 */
export const getStateToken = (
  assets: LucidAssets,
): Effect.Effect<[PolicyId, string], UnauthenticUtxoError> =>
  Effect.gen(function* () {
    const errorMessage = "Failed to get the beacon token from assets";
    const [policyId, assetName, qty] = yield* getSingleAssetApartFromAda(
      assets,
    ).pipe(
      Effect.mapError(
        (e) =>
          new UnauthenticUtxoError({
            message: errorMessage,
            cause: e,
          }),
      ),
    );

    if (qty !== 1n) {
      yield* Effect.fail(
        new UnauthenticUtxoError({
          message: errorMessage,
          cause: `The quantity of the beacon token was expected to be exactly 1, but it was ${qty.toString()}`,
        }),
      );
    }

    return [policyId, assetName];
  });

export type AuthenticUTxO<TDatum, TExtra = undefined> = {
  utxo: UTxO;
  datum: TDatum;
  assetName: string;
} & ([TExtra] extends [undefined] ? {} : TExtra);

export const getDatumFromUTxO = <TDatum>(
  nodeUTxO: UTxO,
  schema: unknown,
): Effect.Effect<TDatum, DataCoercionError> =>
  Effect.gen(function* () {
    const datumCBOR = nodeUTxO.datum;
    if (!datumCBOR) {
      return yield* Effect.fail(
        new DataCoercionError({
          message: `Datum coercion failed`,
          cause: `No datum found`,
        }),
      );
    }

    const datum: TDatum = yield* Effect.try({
      try: () => Data.from(datumCBOR, schema) as TDatum,
      catch: (e) =>
        new DataCoercionError({
          message: `Could not coerce UTxO's datum to the expected datum type`,
          cause: e,
        }),
    });

    return datum;
  });

type AuthenticUTxOBase<TDatum> = {
  utxo: UTxO;
  datum: TDatum;
  assetName: string;
};

const utxoToAuthenticUTxOBase = <TDatum>(
  utxo: UTxO,
  nftPolicy: string,
  schema: unknown,
): Effect.Effect<
  AuthenticUTxOBase<TDatum>,
  DataCoercionError | UnauthenticUtxoError
> =>
  Effect.gen(function* () {
    const datum = yield* getDatumFromUTxO<TDatum>(utxo, schema);
    const [sym, assetName] = yield* getStateToken(utxo.assets);
    if (sym !== nftPolicy) {
      yield* Effect.fail(
        new UnauthenticUtxoError({
          message: `Failed to authenticate UTxO`,
          cause: `UTxO's NFT policy ID is not the same as the expected policy ID`,
        }),
      );
    }

    return { utxo, datum, assetName };
  });

const utxoToAuthenticUTxONoExtra = <TDatum>(
  utxo: UTxO,
  nftPolicy: string,
  schema: unknown,
): Effect.Effect<
  AuthenticUTxO<TDatum>,
  DataCoercionError | UnauthenticUtxoError
> =>
  Effect.map(
    utxoToAuthenticUTxOBase<TDatum>(utxo, nftPolicy, schema),
    (authenticUTxOBase) => {
      const authenticUTxO: AuthenticUTxO<TDatum> = authenticUTxOBase;
      return authenticUTxO;
    },
  );

const utxoToAuthenticUTxOWithExtra = <TDatum, TExtra>(
  utxo: UTxO,
  nftPolicy: string,
  schema: unknown,
  extraFields: (datum: TDatum) => TExtra,
): Effect.Effect<
  AuthenticUTxO<TDatum, TExtra>,
  DataCoercionError | UnauthenticUtxoError
> =>
  Effect.map(
    utxoToAuthenticUTxOBase<TDatum>(utxo, nftPolicy, schema),
    (authenticUTxOBase) => {
      const extra: TExtra = extraFields(authenticUTxOBase.datum);
      const authenticUTxO: AuthenticUTxO<TDatum, TExtra> = {
        ...authenticUTxOBase,
        ...extra,
      };
      return authenticUTxO;
    },
  );

export const authenticateUTxO: {
  <TDatum>(
    utxo: UTxO,
    nftPolicy: string,
    schema: unknown,
  ): Effect.Effect<
    AuthenticUTxO<TDatum>,
    DataCoercionError | UnauthenticUtxoError
  >;
  <TDatum, TExtra>(
    utxo: UTxO,
    nftPolicy: string,
    schema: unknown,
    extraFields: (datum: TDatum) => TExtra,
  ): Effect.Effect<
    AuthenticUTxO<TDatum, TExtra>,
    DataCoercionError | UnauthenticUtxoError
  >;
} = <TDatum, TExtra>(
  utxo: UTxO,
  nftPolicy: string,
  schema: unknown,
  extraFields?: (datum: TDatum) => TExtra,
) => {
  if (extraFields === undefined) {
    return utxoToAuthenticUTxONoExtra<TDatum>(utxo, nftPolicy, schema);
  }

  return utxoToAuthenticUTxOWithExtra<TDatum, TExtra>(
    utxo,
    nftPolicy,
    schema,
    extraFields,
  );
};

export type DroppedUTxO = {
  readonly utxo: UTxO;
  readonly error: { readonly _tag: string; readonly message: string; readonly cause: unknown };
};

/**
 * Logs a warning listing every UTxO that failed conversion, so a caller
 * seeing an empty (or shorter than expected) result can tell "nothing here"
 * apart from "some UTxOs failed to parse/authenticate" by checking the logs,
 * rather than the failures being silently swallowed with no diagnostic path.
 */
export const logDroppedUTxOs = (
  context: string,
  failures: ReadonlyArray<DroppedUTxO>,
): Effect.Effect<void> =>
  failures.length === 0
    ? Effect.void
    : Effect.logWarning(
        `${context}: dropped ${failures.length} invalid UTxO(s)`,
        failures.map(({ utxo, error }) => ({
          utxo: `${utxo.txHash}#${utxo.outputIndex}`,
          tag: error._tag,
          message: error.message,
          cause: error.cause,
        })),
      );

const authenticateAndLogDropped = <TAuth>(
  utxos: UTxO[],
  nftPolicy: string,
  effects: Effect.Effect<TAuth, DataCoercionError | UnauthenticUtxoError>[],
): Effect.Effect<TAuth[]> =>
  Effect.gen(function* () {
    const effectsWithRef = effects.map((effect, i) =>
      effect.pipe(
        Effect.mapError((error) => ({ utxo: utxos[i], error }) as const),
      ),
    );
    const [failures, successes] = yield* Effect.partition(
      effectsWithRef,
      (effect) => effect,
    );
    yield* logDroppedUTxOs(
      `authenticateUTxOs (policy ${nftPolicy})`,
      failures,
    );
    return successes;
  });

/**
 * Drops UTxOs that fail to authenticate (wrong/missing datum, wrong NFT,
 * etc.), logging a warning with each dropped UTxO's reference and error so
 * the failures remain diagnosable rather than silently swallowed.
 */
export const authenticateUTxOs: {
  <TDatum>(
    utxos: UTxO[],
    nftPolicy: string,
    schema: unknown,
  ): Effect.Effect<AuthenticUTxO<TDatum>[]>;
  <TDatum, TExtra>(
    utxos: UTxO[],
    nftPolicy: string,
    schema: unknown,
    extraFields: (datum: TDatum) => TExtra,
  ): Effect.Effect<AuthenticUTxO<TDatum, TExtra>[]>;
} = <TDatum, TExtra>(
  utxos: UTxO[],
  nftPolicy: string,
  schema: unknown,
  extraFields?: (datum: TDatum) => TExtra,
) => {
  if (extraFields === undefined) {
    const effects: Effect.Effect<
      AuthenticUTxO<TDatum>,
      DataCoercionError | UnauthenticUtxoError
    >[] = utxos.map((utxo) =>
      authenticateUTxO<TDatum>(utxo, nftPolicy, schema),
    );
    return authenticateAndLogDropped(utxos, nftPolicy, effects);
  }

  const effects: Effect.Effect<
    AuthenticUTxO<TDatum, TExtra>,
    DataCoercionError | UnauthenticUtxoError
  >[] = utxos.map((utxo) =>
    authenticateUTxO<TDatum, TExtra>(utxo, nftPolicy, schema, extraFields),
  );
  return authenticateAndLogDropped(utxos, nftPolicy, effects);
};

export type FetchSingleAuthenticUTxOConfig<
  TAuthenticUTxO,
  TConversionError,
  TError,
> = {
  address: Address;
  policyId: PolicyId;
  utxoLabel: string;
  conversionFunction: (
    utxos: UTxO[],
    nftPolicy: PolicyId,
  ) => Effect.Effect<TAuthenticUTxO[], TConversionError>;
  onUnexpectedAuthenticUTxOCount: () => TError;
};

export const fetchSingleAuthenticUTxOProgram = <
  TAuthenticUTxO,
  TConversionError,
  TError,
>(
  lucid: LucidEvolution,
  config: FetchSingleAuthenticUTxOConfig<
    TAuthenticUTxO,
    TConversionError,
    TError
  >,
): Effect.Effect<TAuthenticUTxO, LucidError | TConversionError | TError> =>
  Effect.gen(function* () {
    const allUTxOs = yield* Effect.tryPromise({
      try: () => lucid.utxosAt(config.address),
      catch: (e) =>
        new LucidError({
          message: `Failed to fetch the ${config.utxoLabel} UTxO at: ${config.address}`,
          cause: e,
        }),
    });

    const authenticUTxOs = yield* config.conversionFunction(
      allUTxOs,
      config.policyId,
    );

    if (authenticUTxOs.length === 1) {
      return authenticUTxOs[0];
    }

    return yield* Effect.fail(config.onUnexpectedAuthenticUTxOCount());
  });
