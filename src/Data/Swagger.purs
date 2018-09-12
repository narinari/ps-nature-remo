module Data.Swagger where

import Prelude

import Control.Alt ((<|>))
import Data.Foreign.Class (class Decode, decode)
import Data.Foreign.Index (readProp)
import Data.Foreign.NullOrUndefined (NullOrUndefined)
import Data.Lens as L
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.Maybe (Maybe)
import Data.StrMap (StrMap)
import Data.Swagger.JsonSchema (Schema)
import Data.Symbol (SProxy(..))
import Simple.JSON (class ReadForeign, class WriteForeign, readImpl, writeImpl)

type Swagger =
  { paths :: StrMap PathItem
  , definitions :: StrMap Schema }

_paths :: L.Lens' Swagger (StrMap PathItem)
_paths = prop (SProxy :: SProxy "paths")

_definitions :: L.Lens' Swagger (StrMap Schema)
_definitions = prop (SProxy :: SProxy "definitions")

type PathItem =
  { get :: NullOrUndefined Operation
  , put :: NullOrUndefined Operation
  , post :: NullOrUndefined Operation
  , delete :: NullOrUndefined Operation
  , options :: NullOrUndefined Operation
  , head :: NullOrUndefined Operation
  , patch :: NullOrUndefined Operation }

_get :: L.Lens' PathItem (Maybe Operation)
_get = prop (SProxy :: SProxy "get") <<< _Newtype

_put :: L.Lens' PathItem (Maybe Operation)
_put = prop (SProxy :: SProxy "put") <<< _Newtype

_post :: L.Lens' PathItem (Maybe Operation)
_post = prop (SProxy :: SProxy "post") <<< _Newtype

_delete :: L.Lens' PathItem (Maybe Operation)
_delete = prop (SProxy :: SProxy "delete") <<< _Newtype

_options :: L.Lens' PathItem (Maybe Operation)
_options = prop (SProxy :: SProxy "options") <<< _Newtype

_head :: L.Lens' PathItem (Maybe Operation)
_head = prop (SProxy :: SProxy "head") <<< _Newtype

_patch :: L.Lens' PathItem (Maybe Operation)
_patch = prop (SProxy :: SProxy "patch") <<< _Newtype

type Operation =
  { consumes :: NullOrUndefined (Array String)
  , description :: NullOrUndefined String
  , operationId :: NullOrUndefined String
  , produces :: NullOrUndefined (Array String)
  , parameters :: NullOrUndefined (Array Param)
  , responses :: NullOrUndefined (StrMap Response)
  , schemes :: NullOrUndefined (Array String)
  , tags :: NullOrUndefined (Array String)
  }

_consumes :: L.Lens' Operation (Maybe (Array String))
_consumes = prop (SProxy :: SProxy "consumes") <<< _Newtype

_description :: L.Lens' Operation (Maybe String)
_description = prop (SProxy :: SProxy "description") <<< _Newtype

_operationId :: L.Lens' Operation (Maybe String)
_operationId = prop (SProxy :: SProxy "operationId") <<< _Newtype

_produces :: L.Lens' Operation (Maybe (Array String))
_produces = prop (SProxy :: SProxy "produces") <<< _Newtype

_responses :: L.Lens' Operation (Maybe (StrMap Response))
_responses = prop (SProxy :: SProxy "responses") <<< _Newtype

_schemes :: L.Lens' Operation (Maybe (Array String))
_schemes = prop (SProxy :: SProxy "schemes") <<< _Newtype

_tags :: L.Lens' Operation (Maybe (Array String))
_tags = prop (SProxy :: SProxy "tags") <<< _Newtype

type Response =
  { description :: String
  , schema :: NullOrUndefined Schema }

_resDescription :: L.Lens' Response String
_resDescription = prop (SProxy :: SProxy "description")

_schema :: L.Lens' Response (Maybe Schema)
_schema = prop (SProxy :: SProxy "schema") <<< _Newtype

type SwaggerSchema =
  { "$ref" :: NullOrUndefined String
  , "type" :: NullOrUndefined String }

_ref :: L.Lens' SwaggerSchema (Maybe String)
_ref = prop (SProxy :: SProxy "$ref") <<< _Newtype

_type :: L.Lens' SwaggerSchema (Maybe String)
_type = prop (SProxy :: SProxy "type") <<< _Newtype

data Param
  = ParamType
    { "in" :: String
    , "type" :: NullOrUndefined String
    , description :: NullOrUndefined String
    , name :: String
    , required :: NullOrUndefined Boolean
    , schema :: NullOrUndefined Schema
    , uniqueItems :: NullOrUndefined Boolean
    }
  | ParamRef { "$ref" :: String }
instance readForeignParam :: ReadForeign Param where
  readImpl = decode
instance decodeParam :: Decode Param where
  decode f = decode' <|> decodeRef where
    decodeRef = do
      r <- readProp "$ref" f >>= decode
      pure $ ParamRef { "$ref": r }
    decode' = do
      _in <- readProp "in" f >>= decode
      _type <- readProp "type" f >>= decode
      description <- readProp "description" f >>= decode
      name <- readProp "name" f >>= decode
      required <- readProp "required" f >>= decode
      schema <- readProp "schema" f >>= readImpl
      uniqueItems <- readProp "uniqueItems" f >>= decode
      pure $ ParamType
        { "in": _in
        , "type": _type
        , description: description
        , name: name
        , required: required
        , schema: schema
        , uniqueItems: uniqueItems
        }
instance writeForeignParam :: WriteForeign Param where
  writeImpl (ParamType p) = writeImpl p
  writeImpl (ParamRef p) = writeImpl p
