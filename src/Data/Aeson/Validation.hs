{-# LANGUAGE NoImplicitPrelude #-}

-- | JSON schema validation.

module Data.Aeson.Validation
  ( -- * Schema validation
    Schema
  , validate
    -- * Boolean schemas
  , bool
  , true
  , false
    -- * Number schemas
  , number
  , theNumber
  , someNumber
  , integer
  , theInteger
  , someInteger
    -- * String schemas
  , string
  , theString
  , someString
  , regex
  , datetime
    -- * Object schemas
  , Field
  , object
  , object'
  , (.:)
  , (.:?)
    -- * Array schemas (homogenous lists)
  , array
  , sizedArray
  , -- * Set schemas (homogenous, unique lists)
    set
  , sizedSet
    -- * Tuple schemas (heterogeneous lists)
  , tuple
    -- * Miscellaneous schemas
  , anything
  , nullable
  ) where

import Data.Aeson.Validation.Internal.Field (Field)
import Data.Aeson.Validation.Internal.Schema
