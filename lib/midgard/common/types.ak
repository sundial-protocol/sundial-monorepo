use aiken/crypto.{VerificationKeyHash}
use cardano/assets.{AssetName, PolicyId}

/// A number of milliseconds since 00:00:00 UTC on 1 January 1970.
pub type POSIXTime =
  Int

pub type AssetClass {
  policy_id: PolicyId,
  asset_name: AssetName,
}

pub type PubKeyHash =
  VerificationKeyHash
