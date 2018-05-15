module Data.Swagger.JsonSchema where

import Prelude

import Control.Alt ((<|>))
import Data.Foreign (F, ForeignError(..), fail, readString, toForeign)
import Data.Foreign.Class (class Decode, decode)
import Data.Foreign.Index (readProp)
import Data.Foreign.NullOrUndefined (NullOrUndefined(NullOrUndefined))
import Data.Lens as L
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.StrMap (StrMap)
import Data.Symbol (SProxy(..))
import Debug.Trace as Debug
import Simple.JSON (class ReadForeign, class WriteForeign, writeImpl)

newtype Schema = Schema
  { _type :: NullOrUndefined TypeValidator
  , additionalProperties :: NullOrUndefined Schema
  , description :: NullOrUndefined String
  , format :: NullOrUndefined String
  , items :: NullOrUndefined Schema
  , oneOf :: NullOrUndefined (Array Schema)
  , properties :: NullOrUndefined (StrMap Schema)
  , ref :: NullOrUndefined String
  , required :: NullOrUndefined (Array String)
  }
derive instance newtypeSchema :: Newtype Schema _
instance readForeignSchema :: ReadForeign Schema where
  readImpl = decode
instance decodeSchema :: Decode Schema where
  decode f = do
    _type <- readProp "type" f >>= decode
    additionalProperties <- readProp "additionalProperties" f >>= decode
    description <- readProp "description" f >>= decode
    format <- readProp "format" f >>= decode
    items <- readProp "items" f >>= decode
    oneOf <- readProp "oneOf" f >>= decode
    properties <- readProp "properties" f >>= decode
    ref <- readProp "$ref" f >>= decode
    required <- readProp "required" f >>= decode
    pure $ Schema { _type, additionalProperties, description, format, items, oneOf, properties, ref, required }
instance showSchema :: Show Schema where
  show (Schema { _type, additionalProperties, description, format, items, oneOf, properties, ref, required }) =
    "{\n  type: " <> show _type <>
    ",\n  additionalProperties: " <> show additionalProperties <>
    ",\n  description: " <> show description <>
    ",\n  format: " <> show format <>
    ",\n  items: " <> show items <>
    ",\n  oneOf: " <> show oneOf <>
    ",\n  properties: " <> show properties <>
    ",\n  ref: " <> show ref <>
    ",\n  required: " <> show required <> "}"
instance writeForeignSchema :: WriteForeign Schema where
  writeImpl (Schema p) = writeImpl p


_type :: L.Lens' Schema (Maybe TypeValidator)
_type = _Newtype <<< prop (SProxy :: SProxy "_type") <<< _Newtype

_additionalProperties :: L.Lens' Schema (Maybe Schema)
_additionalProperties = _Newtype <<< prop (SProxy :: SProxy "additionalProperties") <<< _Newtype

_description :: L.Lens' Schema (Maybe String)
_description = _Newtype <<< prop (SProxy :: SProxy "description") <<< _Newtype

_format :: L.Lens' Schema (Maybe String)
_format = _Newtype <<< prop (SProxy :: SProxy "format") <<< _Newtype

_items :: L.Lens' Schema (Maybe Schema)
_items = _Newtype <<< prop (SProxy :: SProxy "items") <<< _Newtype

_oneOf :: L.Lens' Schema (Maybe (Array Schema))
_oneOf = _Newtype <<< prop (SProxy :: SProxy "oneOf") <<< _Newtype

_properties :: L.Lens' Schema (Maybe (StrMap Schema))
_properties = _Newtype <<< prop (SProxy :: SProxy "properties") <<< _Newtype

_ref :: L.Lens' Schema (Maybe String)
_ref = _Newtype <<< prop (SProxy :: SProxy "ref") <<< _Newtype

_required :: L.Lens' Schema (Maybe (Array String))
_required = _Newtype <<< prop (SProxy :: SProxy "required") <<< _Newtype

data TypeValidator = TypeValidatorString SchemaType | TypeValidatorArray (Array SchemaType)
instance decodeTypeValidator :: Decode TypeValidator where
  decode f = parseArray <|> parseSingle
    where
      parseArray = TypeValidatorArray <$> (decode f :: F (Array SchemaType))
      parseSingle = TypeValidatorString <$> (decode f :: F SchemaType)
instance showTypeValidator :: Show TypeValidator where
  show (TypeValidatorArray arr) = show arr
  show (TypeValidatorString s) = show s
instance writeForeignTypeValidator :: WriteForeign TypeValidator where
  writeImpl (TypeValidatorString st) = writeImpl st
  writeImpl (TypeValidatorArray st) = writeImpl st

data SchemaType
  = SchemaObject
  | SchemaArray
  | SchemaString
  | SchemaNumber
  | SchemaInteger
  | SchemaBoolean
  | SchemaNull
  | SchemaRef
instance decodeSchemaType :: Decode SchemaType where
  decode f = do
    decoded <- readString f
    case decoded of
      "object" -> pure SchemaObject
      "array" -> pure SchemaArray
      "string" -> pure SchemaString
      "number" -> pure SchemaNumber
      "integer" -> pure SchemaInteger
      "boolean" -> pure SchemaBoolean
      "null" -> pure SchemaNull
      "$ref" -> pure SchemaRef
      _ -> fail (JSONError $ "Could not decode value '" <> "' as type SchemaType")
instance writeForeignSchemaType :: WriteForeign SchemaType where
  writeImpl = toForeign <<< show
instance showSchemaType :: Show SchemaType where
  show SchemaObject = "\"object\""
  show SchemaArray = "\"array\""
  show SchemaString = "\"string\""
  show SchemaNumber = "\"number\""
  show SchemaInteger = "\"integer\""
  show SchemaBoolean = "\"boolean\""
  show SchemaNull = "\"null\""
  show SchemaRef = "\"ref\""
instance eqSchemaType :: Eq SchemaType where
  eq SchemaObject SchemaObject = true
  eq SchemaArray SchemaArray = true
  eq SchemaString SchemaString = true
  eq SchemaNumber SchemaNumber = true
  eq SchemaInteger SchemaInteger = true
  eq SchemaBoolean SchemaBoolean = true
  eq SchemaNull SchemaNull = true
  eq SchemaRef SchemaRef = true
  eq _ _ = false