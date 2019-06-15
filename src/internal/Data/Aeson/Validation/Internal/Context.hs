module Data.Aeson.Validation.Internal.Context
  ( Context(..)
  ) where

import Data.Aeson.Validation.Internal.Prelude


-- Breadcrumbs into a JSON object in reverse order.
data Context
  = Empty
  | Property !Text Context
  | Index !Int Context
