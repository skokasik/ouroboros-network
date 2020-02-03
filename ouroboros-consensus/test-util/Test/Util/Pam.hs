module Test.Util.Pam (
  -- * Pam type
  Pam,
  -- * Query
  Test.Util.Pam.null,
  -- * Construction
  fromMap,
  unsafePam,
  -- * Conversion
  getPam,
  toMap,
  unsafeCoercion,
  -- * Filter
  spanAntitone,
  -- * Min/Max
  minViewWithKey,
  ) where

import           Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Type.Coercion

-- | An inverted 'Map'
--
-- INVARIANT the @k@s are all unique
--
-- INVARIANT the 'NonEmpty's are all ascending
--
newtype Pam v k = UnsafePam {getPam :: Map v (NonEmpty k)}
  deriving (Show)

unsafeCoercion :: Coercion (Pam v k) (Map v (NonEmpty k))
unsafeCoercion = Coercion

unsafePam :: Map v (NonEmpty k) -> Pam v k
unsafePam = UnsafePam

fromMap :: Ord v => Map k v -> Pam v k
fromMap m =
    UnsafePam $ Map.fromListWith (<>) $
    [ (v, k NE.:| []) | (k, v) <- Map.toList m ]

minViewWithKey :: Pam v k -> Maybe ((v, NonEmpty k), Pam v k)
minViewWithKey =
    fmap (fmap UnsafePam) . Map.minViewWithKey . getPam

null :: Pam v k -> Bool
null = Map.null . getPam

spanAntitone :: Ord v => (v -> Bool) -> Pam v k -> (Pam v k, Pam v k)
spanAntitone f (UnsafePam m) = (UnsafePam l, UnsafePam r)
  where
    (l, r) = Map.spanAntitone f m

toMap :: Ord k => Pam v k -> Map k v
toMap (UnsafePam m) =
    Map.fromList $
    [ (k, v) | (v, ks) <- Map.toList m, k <- NE.toList ks ]
