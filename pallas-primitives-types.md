### Haskell codec notes

DecCBOR instances are defined using Cardano.Ledger.Binary.Decoding.Coders.

#### SparseKeyed

Takes a function `Word -> Field a` and a list of required fields and produces a
`Decoder` for `a`. The decoder expects an indefinite or definite length map.
- If indefinite, `applyField` is called until a break is encountered.
- If definite, `applyField` is called a number of times equal to the map length.
`applyField` decodes a `Word` map key and fails if the word has already been
encountered (so maps with duplicate keys always fail to decode). Then it invokes the
update function of the `Field` record returned by the supplied function.

### pallas-primitives/src/lib.rs

```
pub struct PositiveCoin(u64);
```

### pallas-primitives/src/conway/model.rs

```
pub type Multiasset<A> = BTreeMap<PolicyId, BTreeMap<AssetName, A>>;
```

The MultiAsset type itself is not era-parametric in cardano-ledger, but its
decoding depends on the era.

decodeMultiAsset depends on the protocol version:
- If the protocol version is at least 12, then the "djikstra" decoder is used:
  the multiasset must be a nonempty map of nonempty maps with nonzero amounts.
- If the protocol version is at least 9, then the "conway" decoder is used: the
  multiasset may be an empty map, but the values must be nonempty maps with
  nonzero amounts.
- Otherwise, the map may be empty, the values of the map may be empty, and any
  of the amounts may be zero. In this case, any zero amounts are pruned after
  decoding.

```
pub type Mint = Multiasset<NonZeroInt>;
```

Mints in cardano-ledger are directly represented as MultiAsset.

```
#[derive(Serialize, Deserialize, Debug, PartialEq, Eq, Clone)]
pub enum Value {
    Coin(Coin),
    Multiasset(Coin, Multiasset<PositiveCoin>),
}
```

The type family `Value era` is defined as `MaryValue` in eras after Mary;
otherwise, it is defined as `Coin`.

```
instance EncCBOR MaryValue where
  encCBOR (MaryValue c ma@(MultiAsset m)) =
    if Map.null m
      then encCBOR c
      else
        encode $
          Rec MaryValue
            !> To c
            !> To ma
```

The decoder peeks at the first token. If it's an integer, then the decoder
proceeds to decode the full integer and constructs a MaryValue with just the
coin (leaving the multiasset empty). If it's a list (definite or indefinite),
the decoder decodes the (coin, multiasset) value pair as a "record" (using
decodeRecordNamed under the hood). The coin is decoded using decodeWord64 and
the multiasset is decoded using decodeMultiAsset.

All values are decoded using `decodeWord64`. The mary value decoder fails if the
multiasset is too big:

```
isMultiAssetSmallEnough :: MultiAsset -> Bool
isMultiAssetSmallEnough (MultiAsset ma) =
  44 * M.getSum (foldMap' (M.Sum . length) ma) + 28 * length ma <= 65535
```



```
pub type Withdrawals = BTreeMap<RewardAccount, Coin>;
```

The cardano-ledger-core type `Withdrawals` has EncCBOR and DecCBOR derived via
newtype (`Map RewardAccount Coin`).



```
pub type RequiredSigners = NonEmptySet<AddrKeyhash>;
```

This is a `Set (KeyHash 'Witness)` in `ConwayTxBodyRaw`. The `ConwayTxBodyRaw`
decoder enforces that the set is non-empty.



```
#[derive(Encode, Decode, Serialize, Deserialize, Debug, PartialEq, Eq, Clone)]
#[cbor(flat)]
pub enum Certificate {
    ...
}
```

Seems to match ConwayTxCert. The Haskell code defines an ord instance but there
is no Ord on the pallas-primitives type. Will probably want to write plenty of
tests for these. This type is conspicuously missing GenesisDelegCert and
MIRCert. Do these exist in conway?



```
pub enum DRep {
    #[n(0)]
    Key(#[n(0)] AddrKeyhash),
    ...
}
```

I'm not sure what the semantics are for these indices inside of the enum
constructors. Should write tests for this.



```
pub enum Language
```

This looks right



```
pub struct ProtocolParamUpdate {
```

It's a little hard to follow the logic in the cardano-ledger implementation...
There is a field for "protocol version" in the parametric haskell type which is
not present here, but it is of type HKDNoUpdate which makes it isomorphic to
unit (`()`) in an update context.

What happened to fields 12, 13, 14, 15? These correspond to: decentralization,
extraPraosEntropy, protocolVersion, and minUTxOValue, respectively. These were
removed in protocol versions 6, 6, 8, and 4 respectively, which can be seen in
the lenses representing those fields defined in Cardano.Ledger.Shelley.PParams



```
pub struct Update
```

This corresponds to `data Update era`. Encoded as a list of length 2 with fields
`ProposedPPUpdates era` and `EpochNo`. The former field type is a newtype for
`Map (KeyHash 'Genesis) (PParamsUpdate era)` and its EncCBOR is derived.a



```
pub struct PoolVotingThresholds {
...
pub struct DRepVotingThresholds {
```

Looks good, all fields are in the same order. Both use `decodeRecordNamed`.



```
#[derive(Serialize, Deserialize, Encode, Decode, Debug, PartialEq, Clone)]
#[cbor(map)]
pub struct TransactionBody<'a> {
```

Outputs have type `StrictSeq (Sized (TxOut ConwayEra))`; is Sized relevant to
encoding? We use types like `Option<NonEmptySet<T>>` in pallas so that the
derived Decode rejects transactions where the structure is present in the map
but empty, which matches the haskell implementation... In particular, the
Haskell code requires that the following fields are nonempty:
- certificates (4)
- withdrawals (5)
- mint (9)
- collateral inputs (13)
- required signer hashes (14)
- reference inputs (18)
- voting procedures (19)
- proposal procedures (20)
Also, it is enforced that the treasury donation (22) is non-zero. The required
fields are inputs (0), outputs (1), and fee (2).

The pallas code is missing a nonempty constraint for withdrawals (5), mint (9),
and voting procedures (19). It does not enforce that the required fields are
present.

Some optional fields here (collateral return, total collateral, script integrity
hash, aux data hash, network id, current treasury value) are defined using the
type StrictMaybe which is typically decoded using `decodeStrictMaybe`. However,
in the DecCBOR instance for ConwayTxBodyRaw they are decoded using ofield, which
maps a type `a` into a type `StrictMaybe a`. So the default codec for
StrictMaybe (`strict_maybe<a0> = [] / [a0]`) is not involved here.



```
pub enum Vote {
    #[n(0)]
    No,
    #[n(1)]
    Yes,
    #[n(2)]
    Abstain,
}
```

The alternatives of this enum are listed in the correct order.



```
pub type VotingProcedures = BTreeMap<Voter, BTreeMap<GovActionId, VotingProcedure>>;
```

Haskell type is EncCBOR via newtype; DecCBOR uses decodeMapByKey. The sub maps
must not be empty, i.e., if a voter exists in the top-level map it must contain at
least one vote entry.



```
#[derive(Encode, Decode, Serialize, Deserialize, Debug, PartialEq, Eq, Clone)]
pub struct VotingProcedure {
    #[n(0)]
    pub vote: Vote,
    #[n(1)]
    pub anchor: Option<Anchor>,
}
```

The haskell decoder for the anchor field uses decodeNullStrictMaybe, where a
null is decoded as nothing and anything else is decoded exactly as the base type.

```
decodeNullStrictMaybe :: Decoder s a -> Decoder s (StrictMaybe a)
decodeNullStrictMaybe decoder = do
  peekTokenType >>= \case
    C.TypeNull -> do
      decodeNull
      pure SNothing
    _ -> SJust <$> decoder
```



```
#[derive(Encode, Decode, Serialize, Deserialize, Debug, PartialEq, Eq, Clone)]
pub struct ProposalProcedure {
    #[n(0)]
    pub deposit: Coin,
    #[n(1)]
    pub reward_account: RewardAccount,
    #[n(2)]
    pub gov_action: GovAction,
    #[n(3)]
    pub anchor: Anchor,
}
```

The haskell codec is rote here



```
#[derive(Encode, Decode, Serialize, Deserialize, Debug, PartialEq, Eq, Clone)]
#[cbor(flat)]
pub enum GovAction {
    ...
}
```

```
#[derive(Encode, Decode, Serialize, Deserialize, Debug, PartialEq, Eq, Clone)]
pub struct Constitution {
    ...
}
```

Again, all Options are StrictMaybes in haskell and decoded using
decodeNullStrictMaybe.



```
pub enum Voter
```

The haskell type is a sum type with three variants, two of which contain
credentials (sum types of script hash and address key hash). In the codec, the
two credential variants are flattened out so that the type has five variants.
The pallas type is structured similarly so that the codec can be derived. The
discriminators of the variants in pallas match the ones in the haskell codec.



```
pub struct Anchor
pub struct GovActionId
```

Haskell codecs are rote here



```
pub type PostAlonzoTransactionOutput<'b> =
    babbage::GenPostAlonzoTransactionOutput<'b, Value, ScriptRef<'b>>;
pub type TransactionOutput<'b> = babbage::GenTransactionOutput<'b, PostAlonzoTransactionOutput<'b>>;
codec_by_datatype! {
    TransactionOutput<'b>,
    Array | ArrayIndef => Legacy,
    Map | MapIndef => PostAlonzo,
    ()
}
```

These types are just defined as the babbage types. Same for the haskell code:

```
instance Crypto c => EraTxOut (ConwayEra c) where
  type TxOut (BabbageEra c) = BabbageTxOut (BabbageEra c)
```



```
pub struct ExUnitPrices
```

In the haskell code, the fields of `Prices` have type `NonNegativeInterval`,
which is simply a newtype for `BoundedRatio NonNegativeInterval Word64`. The
`DecCBOR` for this type calls `decodeRationalWithTag` and then checks that the
rational is within the bounds (0, maxBound @Word64). `decodeRationalWithTag`
expects tag 30 and then decodes a definite or indefinite list of two integers.
The decoder fails if the denominator is zero.



```
pub enum Redeemers
```

Haskell type: `newtype Redeemers era`. The map key of this type is
`PlutusPurpose AsIx era`; in conway era, this resolves to `ConwayPlutusPurpose
AsIx era`. `ConwayPlutusPurpose` is a HKD type:

```
data ConwayPlutusPurpose f era
  = ConwaySpending !(f Word32 (TxIn (EraCrypto era)))
  | ConwayMinting !(f Word32 (PolicyID (EraCrypto era)))
  | ConwayCertifying !(f Word32 (TxCert era))
  | ConwayRewarding !(f Word32 (RewardAccount (EraCrypto era)))
  | ConwayVoting !(f Word32 (Voter (EraCrypto era)))
  | ConwayProposing !(f Word32 (ProposalProcedure era))
  deriving (Generic)
```

When `f` is instantiated as `AsIx`, the contents of each variant become
equivalent to just `Word32`; i.e. the "item" part is discarded. `AsIx` derives
EncCBOR and DecCBOR via newtype, so `AsIx Word32 a` is encoded and decoded
exactly as `Word32`.

If the protocol version is at least 9, decoder supports a non-empty map
(definite or indefinite) or a non-empty list (definite or indefinite).
Otherwise, it supports a list, which may be empty.

In the map case, each map entry is decoded as a redeemer pointer followed by a
pair-tuple of data and exunits. (How is a pair decoded? I think it's a list;
does it enforce an encoding style?)

In the list case, each list item is decoded via decodeRecordNamed (the record
can be definite or indefinite). The implementation is a little complicated here
because it is generic with respect to the representation of the PlutusPurpose.
In conway era, the record fields are (purpose discriminator, index, data,
exunits). (The first two fields are the fields of the redeemer pointer; they are
flattened here into the 4-tuple via `encCBORGroup`/`decCBORGroup`.



```
pub struct WitnessSet
```

Haskell type: `type TxWits (ConwayEra c) = AlonzoTxWits (ConwayEra c)`.

SparseKeyed decoder with no required fields:
- 0: If the protocol version is at least 9, optionally decodes a set tag and
  then decodes a list of vkey witnesses (list must be non-empty), setting the
  addr tx wits field. Otherwise, there must not be a set tag and the list may be
  empty. Decoder does not fail if duplicates are encountered. Lists may be
  definite or indefinite.
- 1: If the protocol version is at least 9, optionally decodes a set tag and
  then decodes a list of native scripts (list must be non-empty), converting to
  a map scripthash -> script, and appending to the script tx wits field. Decoder
  does not fail if duplicates are encountered. Otherwise, there must not be a
  set tag and the list may be empty. Lists may be definite or indefinite.
- 2: If the protocol version is at least 9, optionally decodes a set tag and
  then decodes a list of BootstrapWitness (list must be non-empty), setting the
  boot addr tx wits field. Otherwise, there must not be a set tag and the list
  may be empty.
- 3: If the protocol version is at least 9, optionally decodes a set tag and
  decodes a list (definite or indefinite) of scripts, converting to a map of
  script hash to script and appending to the script tx wits map. The decoder
  fails if duplicate scripts are encountered or the resulting map is empty. If
  the protocol version is not at least 9, then there must not be a set tag and
  the list may be empty and may contain duplicates. Fails if plutus V1 is not
  legal in this era.
- 4: Decodes a `TxDats era` using `From` coder.
- 5: Decodes a `Redeemers era` using `From` coder.
- 6: Same as 3 but for plutus V2.
- 7: Same as 3 but for plutus V3.



```
pub struct PostAlonzoAuxiliaryData
```

Haskell type: associated type `type TxAuxData era` of class `EraTxAuxData`. For
conway, this is `type TxAuxData (ConwayEra c) = AlonzoTxAuxData (ConwayEra c)`.

```
deriving via
  (Mem AlonzoTxAuxDataRaw era)
  instance
    Era era => DecCBOR (Annotator (AlonzoTxAuxData era))
```

This type is encoded with a tag 259 in alonzo era. The decoder is
backwards-compatible:
- The shelley-format decoder takes a map (definite or indefinite), which is
  treated as the metadata field. In this case, the timelock and plutus script
  fields are left empty.
- The shelleyMA-format decoder takes a list (definite or indefinite), which is
  treated as the above metadata map followed by a list of timelock scripts. In
  this case, the plutus script field is left empty.
- The alonzo-format decoder expects a tag 259, and then decodes a map (definite
  or indefinite) using SparseKeyed. All fields are optional. The value at key 0
  is decoded as the metadata; the value at key 1 is decoded as a list of
  timelock scripts; the values at keys 2, 3, and 4 are decoded as lists of
  plutus scripts.  The decoder permits empty keys in the plutus script map, in
  which case no map entry is created in the decoded haskell type. The decoder
  fails (via guardPlutus) if the map contians plutus scripts for versions that
  are not legal in this era.

Properties that should be tested:
- Pallas decoder supports all formats
- Each format supports definite and indefinite
- Alonzo format expects tag
- All alonzo format fields are optional
- Plutus script map fields may be present but empty



```
pub enum ScriptRef
```

I think this is correct.

This corresponds to `Script era`. In babbage `decodeTxOut`, the script ref field
is decoded as `D $ decodeCIC "Script"`. I think this stands for "Cbor In Cbor".
The function is only ever used to decode the script ref field of a babbage tx
out. The string argument is only used for the error message. It calls
decodeNestedCborBytes...

```
-- | Like `decodeKnownCborDataItem`, but assumes nothing about the Haskell
-- type we want to deserialise back, therefore it yields the `ByteString`
-- Tag 24 surrounded (stripping such tag away).
--
-- In CBOR notation, if the data was serialised as:
--
-- >>> 24(h'DEADBEEF')
--
-- then `decodeNestedCborBytes` yields the inner 'DEADBEEF', unchanged.
decodeNestedCborBytes :: Decoder s BS.ByteString
decodeNestedCborBytes = decodeNestedCborTag >> decodeBytes
```

(where decodeNestedCborTag decodes a tag and asserts that it has value 24.)

...and then it decodes the bytes into the actual type (in this case, `Script era`).

`Script era` is an associated type of class `EraScript`, where `type Script
(ConwayEra c) = AlonzoScript (ConwayEra c)`. The DecCBOR instance for
AlonzoScript uses Summands and a `decodeScript` function that takes an argument
of type `Word`. Summands decodes a sum type from arrays where the first item of
the array represents a discriminator and the remaining items represent the
variant fields. If 0, the remainder is decoded as a TimelockScript (using
`From`); if it is 1-3, the remainder is decoded using `decodePlutusScript` for
the corresponding plutus version. The latter function decodes the script bytes
as binary and then calls mkPlutusScript, which wraps the bytes up as a
PlutusScript if the version is legal in the current era.



```
pub struct Block<'b>
```

I think this is not meant to correspond directly to the haskell node's block
representation:

```
data Block h era
  = Block' !h !(TxSeq era) BSL.ByteString
  deriving (Generic)
```

The fields here are: the header, the transactions, and the encoded bytes of
the block. (The `h` parameter is usually instantiated as either
`BHeader (EraCrypto era)` or `BHeaderView (EraCrypto era)`.)



```
pub struct Tx<'b>
```

In conway era the type instance of `EraTx` class's `Tx era` is `AlonzoTx`:

```
  DecCBOR (Annotator (AlonzoTx era))
  where
  decCBOR =
    decode $
      Ann (RecD AlonzoTx)
        <*! From
        <*! From
        <*! Ann From
        <*! D
          ( sequence . maybeToStrictMaybe
              <$> decodeNullMaybe decCBOR
          )
```

There is an interesting comment about the encoding of AlonzoTx here:

```
--------------------------------------------------------------------------------
-- Mempool Serialisation
--
-- We do not store the Tx bytes for the following reasons:
-- - A Tx serialised in this way never forms part of any hashed structure, hence
--   we do not worry about the serialisation changing and thus seeing a new
--   hash.
-- - The three principal components of this Tx already store their own bytes;
--   here we simply concatenate them. The final component, `IsValid`, is
--   just a flag and very cheap to serialise.
--------------------------------------------------------------------------------

-- | Encode to CBOR for the purposes of transmission from node to node, or from
-- wallet to node.
--
-- Note that this serialisation is neither the serialisation used on-chain
-- (where Txs are deconstructed using segwit), nor the serialisation used for
-- computing the transaction size (which omits the `IsValid` field for
-- compatibility with Mary - see 'toCBORForSizeComputation').
toCBORForMempoolSubmission ::
  ( EncCBOR (TxBody era)
  , EncCBOR (TxWits era)
  , EncCBOR (TxAuxData era)
  ) =>
  AlonzoTx era ->
  Encoding
toCBORForMempoolSubmission
  AlonzoTx {body, wits, auxiliaryData, isValid} =
    encode $
      Rec AlonzoTx
        !> To body
        !> To wits
        !> To isValid
        !> E (encodeNullMaybe encCBOR . strictMaybeToMaybe) auxiliaryData
```

### pallas-primitives/src/alonzo/model.rs

```
#[derive(Serialize, Deserialize, Encode, Decode, Debug, PartialEq, Eq, Clone)]
pub struct TransactionOutput {
    #[n(0)]
    pub address: Bytes,

    #[n(1)]
    pub amount: Value,

    #[n(2)]
    pub datum_hash: Option<DatumHash>,
}
```

So `TxOut era` is an associated type of the class `EraTxOut`. For `ConwayEra`
this resolves to `BabbageTxOut ConwayEra`.

decodeBabbageTxOut peeks at the first token. If it is the start of a definite or
indefinite map, it is decoded using `decodeTxOut`; otherwise it is decoded using
`oldTxOut`.
  - `decodeTxOut` invokes SparseKeyed with an "empty" TxOut, mapping indices 0,
    1, 2, 3 to fields address, value, datum, and script. The address is decoded
    using `fromCborBothAddr`; the value (of type `Value era`) is decoded using
    (`From`); the datum is decoded using `D decCBOR`; the script data is decoded
    using `decodeCIC`. (What is the difference between `From` and `D`?)
  - `oldTxOut` asserts that the next token is the start of a definite or
    indefinite list. If the list doesn't have 2 or 3 elements, the decoder
    fails. If the list has 2 elements, the decoder decodes an address (using
    `fromCborBothAddr`) and then a value (using the decoder for `Value era`
    described above. If the list has 3 elements, the decoder additionally
    decodes a datum hash.
  - the datum hash has type DataHash... which is a synonym for SafeHash, which
    is a newtype for Hash.Hash HASH EraIndependentData that derives EncCBOR and
    DecCBOR... in cardano-crypto-class there are ToCBOR and FromCBOR instances
    for Hash h a... which essentially just convert to and from cbor bytes. The
    decoder asserts that the encoded hash has the correct length.

`fromCborBothAddr` is a little complicated
- If the decoder version is at least 7, we use `decodeAddrRigorous`; otherwise,
  `fromCborBothBackwardsBothAddr`. The difference between the two decoders is
  that the latter decoder permits arbitrary extra garbage after the end of the
  address and permits certain malformed stake reference pointers. Both decoders
  check that the length of the decoded hashes have the correct length and that
  the header byte is valid.
  - The bug where the pre-babbage decoder accepts malformed stake reference
    pointers has to do with the encoding of the pointer fields. The fields of
    the pointers are integers encoded as variable-length sequences of 7-bit
    chunks. However, the integer values of the fields must not be greater than
    32 bits, 16 bits, and 16 bits respectively. The old decoder does not
    actually limit the size of the encoded integer; it simply accumulates the
    chunks and truncates the result to 64 bits.

The pallas implementation just treats addresses as bytes. In particular, it does
not validate that the byte sequence corresponds to a well-formed cardano
address.
