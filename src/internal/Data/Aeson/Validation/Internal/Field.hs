{-# LANGUAGE LambdaCase #-}

module Data.Aeson.Validation.Internal.Field
  ( Field
  , flatten
  ) where

import Data.Aeson.Validation.Internal.Pair
import Data.Aeson.Validation.Internal.Prelude
import Data.Aeson.Validation.Internal.Types

import qualified Data.HashMap.Strict as HashMap
import qualified Data.List.NonEmpty as NonEmpty

flatten :: Strict -> [Field] -> [ShallowField]
flatten s xs =
  mapFields (foldr step mempty xs)
  where
    step
      :: Field
      -> HashMap (Pair Demand Text) (Pair FieldMap [Schema])
      -> HashMap (Pair Demand Text) (Pair FieldMap [Schema])
    step (Field req path sch) =
      go (NonEmpty.toList path)
      where
        go :: [Text]
           -> HashMap (Pair Demand Text) (Pair FieldMap [Schema])
           -> HashMap (Pair Demand Text) (Pair FieldMap [Schema])
        go = \case
          [key] ->
            HashMap.alter
              (\case
                Nothing -> Just (Pair mempty [sch])
                Just (Pair m schs) -> Just (Pair m (sch : schs)))
              (Pair req key)
          key:path' ->
            HashMap.alter
              (\case
                Nothing -> val mempty []
                Just (Pair m schs) -> val m schs)
              (Pair req key)
            where
              val :: FieldMap -> [Schema] -> Maybe (Pair FieldMap [Schema])
              val m ss =
                Just (Pair (FieldMap (go path' (unFieldMap m))) ss)

    mapFields
      :: HashMap (Pair Demand Text) (Pair FieldMap [Schema])
      -> [ShallowField]
    mapFields =
      HashMap.toList >=> go
      where
        go
          :: (Pair Demand Text, Pair FieldMap [Schema])
          -> [ShallowField]
        go (Pair req key, Pair m ss) =
          case mapFields (unFieldMap m) of
            [] -> fields
            fs -> objField fs : fields
          where
            fields :: [ShallowField]
            fields =
              map (ShallowField req key) ss

            objField :: [ShallowField] -> ShallowField
            objField fs =
              ShallowField
                { fieldDemand = req
                , fieldKey    = key
                , fieldSchema = SObject s fs
                }

-- A FieldMap is a temporary data structure used during the conversion from
-- [Field] to [ShallowField].
newtype FieldMap
  = FieldMap
  { unFieldMap :: HashMap (Pair Demand Text) (Pair FieldMap [Schema]) }

instance Semigroup FieldMap where
  (FieldMap x) <> (FieldMap y) = FieldMap (x <> y)

instance Monoid FieldMap where
  mempty = FieldMap mempty
  mappend = (<>)
