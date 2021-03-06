{-# language BangPatterns        #-}
{-# language DeriveGeneric       #-}
{-# language FlexibleContexts    #-}
{-# language FlexibleInstances   #-}
{-# language LambdaCase          #-}
{-# language OverloadedStrings   #-}
{-# language ScopedTypeVariables #-}
{-# language TypeFamilies        #-}

module Data.Aeson.Validation.Internal.Schema
  ( Schema
  , validate
  , bool
  , true
  , false
  , number
  , theNumber
  , someNumber
  , integer
  , theInteger
  , someInteger
  , string
  , theString
  , someString
  , regex
  , datetime
  , object
  , object'
  , (.:)
  , (.:?)
  , array
  , sizedArray
  , set
  , sizedSet
  , tuple
  , anything
  , nullable
  ) where

import Data.Aeson.Validation.Internal.Context
import Data.Aeson.Validation.Internal.Field (flatten)
import Data.Aeson.Validation.Internal.ShallowField (fieldSchemaL)
import Data.Aeson.Validation.Internal.Prelude
import Data.Aeson.Validation.Internal.Types

import Data.Time.ISO8601 (parseISO8601)
import Text.Regex.PCRE.Light (Regex)

import qualified Text.Regex.PCRE.Light as Regex
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.HashMap.Strict as HashMap
import qualified Data.HashSet as HashSet
import qualified Data.Text as Text
import qualified Data.Vector as Vector

-- $setup
-- >>> import Data.Aeson ((.=))

--------------------------------------------------------------------------------
-- Schema smart constructors
--------------------------------------------------------------------------------

-- | Any 'Data.Aeson.Types.Bool'.
--
-- ==== __Examples__
--
-- >>> validate bool (Bool True)
-- []
--
-- >>> validate bool (Bool False)
-- []
bool :: Schema
bool =
  SBool

-- | 'Data.Aeson.Types.Bool' 'True'.
--
-- ==== __Examples__
--
-- >>> validate true (Bool True)
-- []
--
-- >>> validate true (Bool False)
-- ["expected true but found false"]
true :: Schema
true =
  STrue

-- | 'Data.Aeson.Types.Bool' 'False'.
--
-- ==== __Examples__
--
-- >>> validate false (Bool False)
-- []
--
-- >>> validate false (Bool True)
-- ["expected false but found true"]
false :: Schema
false =
  SFalse

-- | Any 'Number'.
--
-- ==== __Examples__
--
-- >>> validate number (Number 1.0)
-- []
number :: Schema
number =
  SNumber

-- | An /approximate/ 'Data.Aeson.Types.Number', with a relative tolerance of
-- @1e-9@ (the smaller number must be within @0.0000001%@ of the larger number).
--
-- You may use a floating point literal instead.
--
-- Here is how you'd implement 'theNumber' using 'someNumber', in case you want
-- to use a different tolerance:
--
-- @
-- 'theNumber' :: 'Scientific' -> 'Schema'
-- 'theNumber' n = 'someNumber' (isClose n)
--   where
--     isClose :: 'Scientific' -> 'Scientific' -> 'Bool'
--     isClose a b = 'abs' (a-b) '<=' 1e-9 * 'max' ('abs' a) ('abs' b)
-- @
--
-- ==== __Examples__
--
-- >>> validate (theNumber 1.5) (Number 1.5)
-- []
--
-- >>> validate 2.5 (Number 2.500000001)
-- []
theNumber :: Scientific -> Schema
theNumber =
  STheNumber

-- | Some 'Number'.
--
-- The 'Text' argument is used for error reporting if validation fails.
--
-- ==== __Examples__
--
-- >>> validate (someNumber "greater than 5" (> 5)) (Number 4)
-- ["failed predicate: greater than 5"]
someNumber :: Text -> (Scientific -> Bool) -> Schema
someNumber =
  SSomeNumber

-- | Any integer 'Number'.
--
-- ==== __Examples__
--
-- >>> validate integer (Number 1.0)
-- []
--
-- >>> validate integer (Number 1.5)
-- ["expected an integer but found 1.5"]
integer :: Schema
integer =
  SInteger

-- | An exact integer 'Data.Aeson.Types.Number'.
--
-- You may use an integer literal instead.
--
-- ==== __Examples__
--
-- >>> validate (theInteger 1) (Number 1)
-- []
--
-- >>> validate 1 (Number 2)
-- ["expected 1 but found 2.0"]
theInteger :: Integer -> Schema
theInteger =
  STheInteger

-- | Some integer 'Number'.
--
-- The 'Text' argument is used for error reporting if validation fails.
--
-- ==== __Examples__
--
-- >>> validate (someInteger "greater than 5" (> 5)) (Number 6)
-- []
--
-- >>> validate (someInteger "greater than 5" (> 5)) (Number 6.5)
-- ["failed predicate: greater than 5"]
someInteger :: Text -> (Integer -> Bool) -> Schema
someInteger s p =
  SSomeNumber s (either nope p . floatingOrInteger)
  where
    nope :: Double -> Bool
    nope _ = False

-- | Any 'Data.Aeson.Types.String'.
--
-- ==== __Examples__
--
-- >>> validate string (String "foo")
-- []
--
-- >>> validate string (Bool True)
-- ["expected a string but found a bool"]
string :: Schema
string =
  SString

-- | An exact 'Data.Aeson.Types.String'.
--
-- You may use a string literal instead (requires @-XOverloadedStrings@).
--
-- ==== __Examples__
--
-- >>> validate (theString "foo") (String "foo")
-- []
--
-- >>> validate "foo" (String "bar")
-- ["expected \"foo\" but found \"bar\""]
theString :: Text -> Schema
theString =
  STheString

-- | Some 'Data.Aeson.Types.String'.
--
-- The 'Text' argument is used for error reporting if validation fails.
--
-- ==== __Examples__
--
-- >>> validate (someString "len > 5" (\s -> Text.length s > 5)) (String "foobar")
-- []
--
-- >>> validate (someString "len > 5" (\s -> Text.length s > 5)) (String "foo")
-- ["failed predicate: len > 5"]
someString :: Text -> (Text -> Bool) -> Schema
someString =
  SSomeString

-- | A 'Data.Aeson.Types.String' that matches a regular expression.
--
-- ==== __Examples__
--
-- >>> validate (regex "a+b") (String "xaaabx")
-- []
--
-- >>> validate (regex "c{2}") (String "cd")
-- ["failed predicate: matches c{2}"]
regex :: Text -> Schema
regex r =
  SSomeString ("matches " <> r) p
  where
    p :: Text -> Bool
    p s =
      has (_Just . _head) (Regex.match r' (encodeUtf8 s) [])

    r' :: Regex
    r' =
      Regex.compile (encodeUtf8 r) [Regex.utf8]

-- | A 'Data.Aeson.Types.String' in
-- <https://www.ietf.org/rfc/rfc3339.txt ISO 8601> format.
--
-- ==== __Examples__
-- >>> validate datetime (String "2000-01-01T00:00:00.000Z")
-- []
datetime :: Schema
datetime =
  SDateTime

-- | An 'Object', possibly with additional fields.
--
-- To match any 'Object', use @'object' []@.
--
-- ==== __Examples__
--
-- >>> let fields = [("foo":|[]) .: number]
-- >>> let values = ["foo" .= Number 1, "bar" .= Bool True]
-- >>> validate (object fields) (Object values)
-- []
--
-- >>> let fields = [("foo":|["bar"]) .: number]
-- >>> let values = ["foo" .= Object ["bar" .= Number 1]]
-- >>> validate (object fields) (Object values)
-- []
object :: [Field] -> Schema
object xs =
  SObject NotStrict (flatten NotStrict xs)

-- | An 'Object' with no additional fields.
--
-- The @'@ mark means \"strict\" as in @foldl'@, because 'object'' matches
-- 'Object's more strictly than 'object'.
--
-- ==== __Examples__
--
-- >>> let fields = [("foo":|[]) .: number]
-- >>> let values = ["foo" .= Number 1]
-- >>> validate (object' fields) (Object values)
-- []
--
-- >>> let fields = [("foo":|[]) .: number]
-- >>> let values = ["foo" .= Number 1, "bar" .= Bool True]
-- >>> validate (object' fields) (Object values)
-- ["extra fields: \"bar\""]
--
-- >>> let fields = [("foo":|["bar"]) .: number]
-- >>> let values = ["foo" .= Object ["bar" .= Number 1]]
-- >>> validate (object' fields) (Object values)
-- []
--
-- >>> let fields = [("foo":|["bar"]) .: number]
-- >>> let values = ["foo" .= Object ["bar" .= Number 1], "baz" .= Bool True]
-- >>> validate (object' fields) (Object values)
-- ["extra fields: \"baz\""]
object' :: [Field] -> Schema
object' xs =
  SObject Strict (flatten Strict xs)

-- | A required 'Field'.
(.:) :: NonEmpty Text -> Schema -> Field
(.:) =
  Field Req
infixr 5 .:

-- | An optional 'Field'.
(.:?) :: NonEmpty Text -> Schema -> Field
(.:?) =
  Field Opt
infixr 5 .:?

-- | A "homogenous" 'Array' of any size.
--
-- The array need not be /truly/ homogenous; it simply has the same 'Schema'
-- applied to each element. However, the 'Schema' could be 'anything', or
-- composed of many alternatives using '<>'.
--
-- ==== __Examples__
--
-- >>> validate (array bool) (Array [Bool True, Bool False])
-- []
--
-- >>> validate (array anything) (Array [Bool True, String "foo"])
-- []
--
-- >>> validate (array integer) (Array [Number 1.5])
-- ["in context 0, expected an integer but found 1.5"]
array :: Schema -> Schema
array =
  SArray NotUnique minBound maxBound

-- | A sized (inclusive), "homogenous" (see note above) 'Array'. Use 'minBound'
-- or 'maxBound' for an unbounded edge.
--
-- ==== __Examples__
--
-- >>> validate (sizedArray 1 2 bool) (Array [Bool True])
-- []
--
-- >>> validate (sizedArray 1 2 bool) (Array [Bool True, Bool True, Bool False])
-- ["expected between 1 and 2 elements (inclusive) but found 3"]
sizedArray :: Int -> Int -> Schema -> Schema
sizedArray =
  SArray NotUnique

-- | A "homogenous" (see note above), unique 'Array' of any size.
--
-- ==== __Examples__
--
-- >>> validate (set bool) (Array [Bool True])
-- []
--
-- >>> validate (set bool) (Array [Bool True, Bool True])
-- ["array does not contain unique elements"]
set :: Schema -> Schema
set =
  SArray Unique minBound maxBound

-- | A sized (inclusive), "homogenous" (see note above), unique 'Array'. Use
-- 'minBound' or 'maxBound' for an unbounded edge.
--
-- ==== __Examples__
--
-- >>> validate (sizedSet 1 1 string) (Array [String "foo"])
-- []
--
-- >>> validate (sizedSet 1 1 string) (Array [String "foo", String "bar"])
-- ["expected between 1 and 1 elements (inclusive) but found 2"]
sizedSet :: Int -> Int -> Schema -> Schema
sizedSet =
  SArray Unique

-- | A heterogeneous 'Array' exactly as long as the given list of 'Schema's.
--
-- ==== __Examples__
--
-- >>> validate (tuple [bool, string]) (Array [Bool True, String "foo"])
-- []
tuple :: [Schema] -> Schema
tuple =
  STuple

-- | Any 'Value' whatsoever, including 'Null'.
--
-- ==== __Examples__
--
-- >>> validate anything (Bool True)
-- []
--
-- >>> validate anything Null
-- []
anything :: Schema
anything =
  SAnything

-- | Modify a 'Schema' to additionally accept 'Null'.
--
-- 'nullable' is idempotent:
--
-- @
-- 'nullable' = 'nullable' . 'nullable'
-- @
--
-- ==== __Examples__
--
-- >>> validate (nullable bool) (Bool True)
-- []
--
-- >>> validate (nullable bool) Null
-- []
--
-- >>> validate (nullable bool) (String "foo")
-- ["expected a bool but found a string"]
nullable :: Schema -> Schema
nullable =
  SNullable


normalize :: Schema -> Schema
normalize = transform
  (\case
    SAlts ss ->
      SAlts (NonEmpty.fromList (NonEmpty.toList ss >>= unAlt))
    s -> s)
 where
  -- Rip off top-level SAlts constructor
  unAlt :: Schema -> [Schema]
  unAlt (SAlts ss) = NonEmpty.toList ss
  unAlt s = [s]


--------------------------------------------------------------------------------
-- Validation
--------------------------------------------------------------------------------

-- | Validate a 'Value' with a 'Schema' and emit schema violations as 'Text'.
validate :: Schema -> Value -> [Text]
validate s v =
  toList (runReader (validate_ (normalize s) v) Empty)

validate_
  :: (Applicative m, MonadReader Context m)
  => Schema
  -> Value
  -> m (Seq Text)
validate_ = \case
  SBool           -> validateBool
  STrue           -> validateTrue
  SFalse          -> validateFalse
  SNumber         -> validateNumber
  SInteger        -> validateInteger
  STheNumber n    -> validateTheNumber n
  STheInteger n   -> validateTheInteger n
  SSomeNumber s p -> validateSomeNumber s p
  SString         -> validateString
  STheString s    -> validateTheString s
  SSomeString p s -> validateSomeString p s
  SDateTime       -> validateDateTime
  SObject s xs    -> validateObject s xs
  SArray u x y s  -> validateArray u x y s
  STuple ss       -> validateTuple ss
  SAnything       -> const (pure mempty)
  SNullable s     -> validateNullable s
  SAlts ss        -> validateAlts ss
  SNegate s0      -> case s0 of
    SBool           -> validateNotBool
    STrue           -> validateNotTrue
    SFalse          -> validateNotFalse
    SNumber         -> validateNotNumber
    SInteger        -> validateNotInteger
    STheNumber n    -> validateNotTheNumber n
    STheInteger n   -> validateNotTheInteger n
    SSomeNumber s p -> validateNotSomeNumber s p
    SString         -> validateNotString
    STheString s    -> validateNotTheString s
    SSomeString s p -> validateNotSomeString s p
    SDateTime       -> validateNotDateTime
    SObject s xs    -> validateNotObject s xs
    SArray u x y s  -> validateNotArray u x y s
    STuple ss       -> validateNotTuple ss
    SAnything       -> const (err "negate anything")
    SNullable s     -> validateNotNullable s
    SAlts ss        -> validateNotAlts ss
    SNegate s       -> validate_ s

validateBool :: (Applicative m, MonadReader Context m) => Value -> m (Seq Text)
validateBool = \case
  Bool _ -> pure mempty
  v -> mismatch "a bool" (valType v)

validateTrue :: (Applicative m, MonadReader Context m) => Value -> m (Seq Text)
validateTrue = \case
  Bool b | b -> pure mempty
         | otherwise -> mismatch "true" "false"
  v -> mismatch "true" (valType v)

validateFalse :: (Applicative m, MonadReader Context m) => Value -> m (Seq Text)
validateFalse = \case
  Bool b | b -> mismatch "false" "true"
         | otherwise -> pure mempty
  v -> mismatch "false" (valType v)

validateNumber
  :: (Applicative m, MonadReader Context m) => Value -> m (Seq Text)
validateNumber = \case
  Number _ -> pure mempty
  v -> mismatch "a number" (valType v)

validateInteger
  :: (Applicative m, MonadReader Context m) => Value -> m (Seq Text)
validateInteger = \case
  Number n | isInteger n -> pure mempty
           | otherwise -> mismatch "an integer" (tshow n)
  v -> mismatch "an integer" (valType v)

validateTheNumber
  :: (Applicative m, MonadReader Context m)
  => Scientific -> Value -> m (Seq Text)
validateTheNumber n = \case
  Number m | isclose n m -> pure mempty
           | otherwise -> mismatch (tshow n) (tshow m)
  v -> mismatch (tshow n) (valType v)

validateTheInteger
  :: (Applicative m, MonadReader Context m) => Integer -> Value -> m (Seq Text)
validateTheInteger n = \case
  Number m | fromInteger n == m -> pure mempty
           | otherwise -> mismatch (tshow n) (tshow m)
  v -> mismatch (tshow n) (valType v)

validateSomeNumber
  :: (Applicative m, MonadReader Context m)
  => Text
  -> (Scientific -> Bool)
  -> Value
  -> m (Seq Text)
validateSomeNumber msg p = \case
  Number n | p n -> pure mempty
           | otherwise -> failedPredicate msg
  v -> mismatch "a number" (valType v)

validateString
  :: (Applicative m, MonadReader Context m)
  => Value -> m (Seq Text)
validateString = \case
  String _ -> pure mempty
  v -> mismatch "a string" (valType v)

validateTheString
  :: (Applicative m, MonadReader Context m)
  => Text -> Value -> m (Seq Text)
validateTheString s = \case
  String s' | s == s' -> pure mempty
            | otherwise -> mismatch (tshow s) (tshow s')
  v -> mismatch (tshow s) (valType v)

validateSomeString
  :: (Applicative m, MonadReader Context m)
  => Text
  -> (Text -> Bool)
  -> Value
  -> m (Seq Text)
validateSomeString msg p = \case
  String s | p s -> pure mempty
           | otherwise -> failedPredicate msg
  v -> mismatch "a string" (valType v)

validateDateTime
  :: (Applicative m, MonadReader Context m) => Value -> m (Seq Text)
validateDateTime = \case
  String s | isDateTime s -> pure mempty
           | otherwise -> mismatch "a datetime" (tshow s)
  v -> mismatch "a string" (valType v)

validateObject
  :: (Applicative m, MonadReader Context m)
  => Strict -> [ShallowField] -> Value -> m (Seq Text)
validateObject s xs = \case
  Object obj ->
    case s of
      NotStrict -> mconcat <$> mapM (validateField obj) xs
      Strict    -> validateObject' xs obj
  v -> mismatch "an object" (valType v)

validateObject'
  :: (Applicative m, MonadReader Context m)
  => [ShallowField] -> HashMap Text Value -> m (Seq Text)
validateObject' [] obj =
  case HashMap.keys obj of
    [] -> pure mempty
    ks -> err ("extra fields: " <> Text.intercalate ", " (map tshow ks))
validateObject' (x:xs) obj = (<>)
  <$> validateField obj x
  <*> validateObject' xs (HashMap.delete (fieldKey x) obj)

validateField
  :: (Applicative m, MonadReader Context m)
  => HashMap Text Value -> ShallowField -> m (Seq Text)
validateField obj = \case
  ShallowField Req key s ->
    case HashMap.lookup key obj of
      Nothing -> err ("missing field " <> tshow key)
      Just v  -> local (Property key) (validate_ s v)
  ShallowField Opt key s ->
    case HashMap.lookup key obj of
      Nothing -> pure mempty
      Just v  -> local (Property key) (validate_ s v)

validateArray
  :: forall m. (Applicative m, MonadReader Context m)
  => Unique -> Int -> Int -> Schema -> Value -> m (Seq Text)
validateArray uniq x y sch = \case
  Array v -> do
    errs1 <-
      case sch of
        SAnything -> pure mempty
        _ ->
          mconcat <$>
            imapM (\n val -> local (Index n) (validate_ sch val))
              (Vector.toList v)

    errs2 <-
      if uniq == Unique
        then uniqCheck v
        else pure mempty

    errs3 <- boundsCheck (length v)

    pure (errs1 <> errs2 <> errs3)
  v -> mismatch "an array" (valType v)
 where
  boundsCheck :: Int -> m (Seq Text)
  boundsCheck len
    | len >= x && len <= y = pure mempty
    | otherwise =
        case (x == minBound, y == maxBound) of
          (True, True) -> error "impossible"
          (True, False) -> mismatch ("<= " <> tshow y <> " elements") (tshow len)
          (False, True) -> mismatch (">= " <> tshow x <> " elements") (tshow len)
          (False, False) ->
            mismatch
              ("between " <> tshow x <> " and " <> tshow y <>
                " elements (inclusive)")
              (tshow len)

  uniqCheck :: Vector Value -> m (Seq Text)
  uniqCheck v =
    if length v /= length (toSet v)
       then err "array does not contain unique elements"
       else pure mempty
   where
    toSet :: Vector Value -> HashSet Value
    toSet = Vector.foldr' HashSet.insert mempty

validateTuple
  :: forall m.
     (Applicative m, MonadReader Context m)
  => [Schema] -> Value -> m (Seq Text)
validateTuple ss0 = \case
  Array v0 -> go 0 ss0 (toList v0)
   where
    go :: Int -> [Schema] -> [Value] -> m (Seq Text)
    go _  [] [] = pure mempty
    go !n (s:ss) (v:vs) = (<>)
      <$> local (Index n) (validate_ s v)
      <*> go (n+1) ss vs
    go _ _ _ =
      mismatch (tshow (length ss0) <> " elements")
        (tshow (length v0) <> " elements")
  v -> mismatch "an array" (valType v)

validateNullable
  :: (Applicative m, MonadReader Context m)
  => Schema -> Value -> m (Seq Text)
validateNullable s v =
  if v /= Null
    then validate_ s v
    else pure mempty

validateAlts
  :: forall m.
     (Applicative m, MonadReader Context m)
  => NonEmpty Schema -> Value -> m (Seq Text)
validateAlts ss0 val = go (NonEmpty.toList ss0)
  where
    go :: [Schema] -> m (Seq Text)
    go [] = error "impossible"
    go [s] = validate_ s val
    go (s:ss) = do
      errs1 <- validate_ s val
      errs2 <- go ss
      if null errs1 || null errs2
        then pure mempty
        else pure (errs1 <> errs2)

validateNotBool
  :: (Applicative m, MonadReader Context m) => Value -> m (Seq Text)
validateNotBool v =
  case v of
    Bool b -> mismatch "anything but a bool" (if b then "true" else "false")
    _ -> pure mempty

validateNotTrue
  :: (Applicative m, MonadReader Context m) => Value -> m (Seq Text)
validateNotTrue = \case
  Bool True -> mismatch "anything but true" "true"
  _ -> pure mempty

validateNotFalse
  :: (Applicative m, MonadReader Context m) => Value -> m (Seq Text)
validateNotFalse = \case
  Bool False -> mismatch "anything but false" "false"
  _ -> pure mempty

validateNotNumber
  :: (Applicative m, MonadReader Context m) => Value -> m (Seq Text)
validateNotNumber v =
  case v of
    Number n -> mismatch "anything but a number" (tshow n)
    _ -> pure mempty

validateNotInteger
  :: (Applicative m, MonadReader Context m) => Value -> m (Seq Text)
validateNotInteger = \case
  Number n | isInteger n ->
    mismatch "anything but an integer" (tshow m)
    where
      m :: Integer
      m = either nope id (floatingOrInteger n)

      nope :: Double -> Integer
      nope = error "impossible"
  _ ->
    pure mempty

validateNotTheNumber
  :: (Applicative m, MonadReader Context m)
  => Scientific -> Value -> m (Seq Text)
validateNotTheNumber n v =
  case v of
    Number m | isclose n m ->
      mismatch ("anything but " <> tshow n) (tshow m)
    _ ->
      pure mempty

validateNotTheInteger
  :: (Applicative m, MonadReader Context m)
  => Integer -> Value -> m (Seq Text)
validateNotTheInteger n v =
  case v of
    Number m | fromInteger n == m ->
      mismatch ("anything but " <> tshow n) (tshow m)
    _ ->
      pure mempty

validateNotSomeNumber
  :: (Applicative m, MonadReader Context m)
  => Text
  -> (Scientific -> Bool)
  -> Value
  -> m (Seq Text)
validateNotSomeNumber msg p = \case
  Number n | p n ->
    passedPredicate msg
  _ ->
    pure mempty

validateNotString
  :: (Applicative m, MonadReader Context m)
  => Value -> m (Seq Text)
validateNotString v =
  case v of
    String s ->
      mismatch "anything but a string" (tshow s)
    _ ->
      pure mempty

validateNotTheString
  :: (Applicative m, MonadReader Context m)
  => Text -> Value -> m (Seq Text)
validateNotTheString s = \case
  String s' | s == s' ->
    mismatch ("anything but " <> tshow s) (tshow s)
  _ ->
    pure mempty

validateNotSomeString
  :: (Applicative m, MonadReader Context m)
  => Text
  -> (Text -> Bool)
  -> Value
  -> m (Seq Text)
validateNotSomeString msg p = \case
  String s | p s ->
    passedPredicate msg
  _ ->
    pure mempty

validateNotDateTime
  :: (Applicative m, MonadReader Context m)
  => Value -> m (Seq Text)
validateNotDateTime = \case
  String s | isDateTime s ->
    mismatch "anything but a datetime" (tshow s)
  _ ->
    pure mempty

validateNotObject
  :: (Applicative m, MonadReader Context m)
  => Strict -> [ShallowField] -> Value -> m (Seq Text)
validateNotObject sch xs val = do
  errs <- validateObject sch xs val
  if null errs
    then err "passed object schema"
    else pure mempty

validateNotArray
  :: (Applicative m, MonadReader Context m)
  => Unique -> Int -> Int -> Schema -> Value -> m (Seq Text)
validateNotArray uniq x y sch val = do
  errs <- validateArray uniq x y sch val
  if null errs
    then err ("passed " <> typ <> " schema")
    else pure mempty
  where
    typ :: Text
    typ =
      case uniq of
        Unique    -> "set"
        NotUnique -> "array"

validateNotTuple
  :: (Applicative m, MonadReader Context m)
  => [Schema] -> Value -> m (Seq Text)
validateNotTuple ss val = do
  errs <- validateTuple ss val
  if null errs
    then err "passed tuple schema"
    else pure mempty

validateNotNullable
  :: (Applicative m, MonadReader Context m)
  => Schema -> Value -> m (Seq Text)
validateNotNullable s = \case
  Null -> err "passed nullable schema"
  v    -> validate_ (negate s) v

validateNotAlts
  :: (Applicative m, MonadReader Context m)
  => NonEmpty Schema -> Value -> m (Seq Text)
validateNotAlts ss val =
  mconcat <$> mapM (\s -> validate_ (negate s) val) (toList ss)

-- Validation error; prefixes the message with the current context (if any).
err
  :: (Applicative m, MonadReader Context m)
  => Text -> m (Seq Text)
err msg = do
  ctx <- ask
  pure (pure (prefix ctx <> msg))
  where
    prefix :: Context -> Text
    prefix =
      maybe mempty (\s -> "in context " <> s <> ", ") . prettyPrintContext

    -- |
    -- >>> prettyPrintContext Empty
    -- Nothing
    --
    -- >>> prettyPrintContext (Property "foo" (Index 1 (Property "bar" Empty)))
    -- Just "\"bar\".1.\"foo\""
    prettyPrintContext :: Context -> Maybe Text
    prettyPrintContext = \case
      Empty            -> Nothing
      Property key ctx -> Just (go ctx <> tshow key)
      Index    idx ctx -> Just (go ctx <> tshow idx)
      where
        go :: Context -> Text
        go = \case
          Empty            -> ""
          Property key ctx -> go ctx <> tshow key <> "."
          Index    idx ctx -> go ctx <> tshow idx <> "."

-- Generic "mismatch" of the expected/actual sort.
mismatch
  :: ( Applicative m
     , MonadReader Context m
     )
  => Text
  -> Text
  -> m (Seq Text)
mismatch x y =
  err ("expected " <> x <> " but found " <> y)

-- Erroneously failed a named predicate.
failedPredicate
  :: ( Applicative m
     , MonadReader Context m
     )
  => Text
  -> m (Seq Text)
failedPredicate msg =
  err ("failed predicate: " <> msg)

-- Erroneously passed a named predicate.
passedPredicate
  :: ( Applicative m
     , MonadReader Context m
     )
  => Text
  -> m (Seq Text)
passedPredicate msg =
  err ("passed predicate: " <> msg)

valType :: Value -> Text
valType = \case
  Null     -> "null"
  Bool   _ -> "a bool"
  Number _ -> "a number"
  String _ -> "a string"
  Object _ -> "an object"
  Array  _ -> "an array"

tshow :: Show a => a -> Text
tshow = Text.pack . show

isclose :: Scientific -> Scientific -> Bool
isclose n m = abs (n-m) <= 1e-9 * max (abs n) (abs m)

isDateTime :: Text -> Bool
isDateTime = isJust . parseISO8601 . Text.unpack

imapM :: Monad m => (Int -> a -> m b) -> [a] -> m [b]
imapM f xs = mapM (uncurry f) (zip [0..] xs)



--------------------------------------------------------------------------------
-- Uniplate stuff, not enough to be worth the dependency
--------------------------------------------------------------------------------

{-
universe :: Schema -> [Schema]
universe s =
  s : case s of
    SAlts x        -> NonEmpty.toList x >>= universe
    SArray _ _ _ x -> universe x
    SNegate x      -> universe x
    SNullable x    -> universe x
    SObject _ x    -> x >>= universe . fieldSchema
    STuple x       -> x >>= universe

    SAnything     -> []
    SBool         -> []
    SDateTime     -> []
    SFalse        -> []
    SInteger      -> []
    SNumber       -> []
    SSomeNumber{} -> []
    SSomeString{} -> []
    SString       -> []
    STheInteger{} -> []
    STheNumber{}  -> []
    STheString{}  -> []
    STrue         -> []
-}

-- Transform all 'Schema', bottom up.
transform :: (Schema -> Schema) -> Schema -> Schema
transform f =
  f . go (transform f)
  where
    -- Transform direct children.
    go :: (Schema -> Schema) -> Schema -> Schema
    go f s =
      case s of
        SAlts a        -> SAlts (fmap f a)
        SArray a b c d -> SArray a b c (f d)
        SNegate a      -> SNegate (f a)
        SNullable a    -> SNullable (f a)
        SObject a b    -> SObject a (b & each.fieldSchemaL %~ f)
        STuple a       -> STuple (map f a)

        SAnything     -> s
        SBool         -> s
        SDateTime     -> s
        SFalse        -> s
        SInteger      -> s
        SNumber       -> s
        SSomeNumber{} -> s
        SSomeString{} -> s
        SString       -> s
        STheInteger{} -> s
        STheNumber{}  -> s
        STheString{}  -> s
        STrue         -> s
