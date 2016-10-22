{-# LANGUAGE BangPatterns #-}

module Data.Aeson.Validation.Types where

import Data.Text       (Text)
import Data.Scientific (Scientific)

-- | An opaque object 'Field'.
--
-- Create a 'Field' with '.:' or '.:?', and bundle it into a 'Schema' using
-- 'object' or 'object''
data Field
  = ReqField !Text Schema
  | OptField !Text Schema

-- | An opaque JSON 'Schema'.
data Schema
  = SBool
  | STrue
  | SFalse
  | SNumber
  | SInteger
  | SSomeNumber (Scientific -> Bool) (Maybe Text) {- error msg -}
  | SString
  | STheString Text
  | SSomeString (Text -> Bool) (Maybe Text) {- error msg -}
  | SObject Strict [Field]
  | SArray Unique Int {- min len -} !Int {- max len -} Schema
  | STuple [Schema]
  | SAnything
  | SNullable Schema
  | SAlt Schema Schema
  | SInverse Schema

-- | Are extra properties of an object allowed?
data Strict
  = Strict
  | NotStrict

-- | Are duplicate elements in an array allowed?
data Unique
  = Unique
  | NotUnique

-- | Breadcrumbs into a JSON object in reverse order.
data Context
  = Empty
  | Property Context !Text
  | Index Context !Int
