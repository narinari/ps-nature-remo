module NatureRemo.Generation.Names where

import Prelude

import Data.Array as Array
import Data.Either (Either(..))
import Data.Foldable (find, any)
import Data.List.NonEmpty as NonEmptyList
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String as Str
import Data.String.Regex as Regex
import Data.String.Regex.Flags as RegexFlags
import Data.Swagger.AST (ApiModuleName)

type FieldMapping = {json :: String, ps :: String}

typeUnqualifiedName :: String -> String
typeUnqualifiedName full = fromMaybe full finalName
  where
    finalName = Array.last $ Str.split (Str.Pattern ".") full

typeQualifiedName :: String -> String
typeQualifiedName full = fromMaybe full (mkName <$> typeModule full <*> finalName)
  where
    mkName mod name = mod <> "." <> name
    finalName = Array.last $ Str.split (Str.Pattern ".") full

refName :: String -> String -> String
refName moduleName fullRefName | (Just moduleName) == typeModule fullRefName = typeUnqualifiedName fullRefName
refName _ fullRefName = typeQualifiedName fullRefName

refName' :: String -> String -> String
refName' moduleName qualifiedRefName | (Just moduleName) == typeModule' qualifiedRefName = typeUnqualifiedName qualifiedRefName
refName' _ qualifiedRefName = qualifiedRefName

typeModule :: String -> Maybe String
typeModule fullyQualifiedName = stripPrefixes fullyQualifiedName
  where
    stripPrefixes s = Array.last $ Str.split (Str.Pattern "/") s

typeModule' :: String -> Maybe String
typeModule' qualifiedName = case Str.split (Str.Pattern ".") qualifiedName of
  [moduleName, typeName] -> Just moduleName
  _ -> Nothing

modulePrefix :: String -> String
modulePrefix = Str.split (Str.Pattern ".") >>> Array.dropEnd 1 >>> Str.joinWith "."

startsWith :: String -> String -> Boolean
startsWith prefix str = case Regex.regex ("^" <> prefix) RegexFlags.noFlags of
  Left _ -> false
  Right matcher -> Regex.test matcher str

endsWith :: String -> String -> Boolean
endsWith suffix str = case Regex.regex (suffix <> "$") RegexFlags.noFlags of
  Left _ -> false
  Right matcher -> Regex.test matcher str

reservedKeywords :: Array FieldMapping
reservedKeywords =
  [ {json: "$ref", ps: "_ref"}
  , {json: "$schema", ps: "_schema"}
  , {json: "type", ps: "_type"}
  , {json: "data", ps: "_data"}
  , {json: "default", ps: "_default"} ]

jsonFieldToPsField :: String -> String
jsonFieldToPsField f =
  case find ((==) f <<< _.json) reservedKeywords of
    Just {ps} -> ps
    Nothing -> if startsWithUpperCase f then "_" <> f else f

psFieldToJsonField :: String -> String
psFieldToJsonField f =
  case find ((==) f <<< _.ps) reservedKeywords of
    Just {json} -> json
    Nothing -> if startsWith "_" f then Str.drop 1 f else f

lowercaseFirstChar :: String -> String
lowercaseFirstChar str = case Str.splitAt 1 str of
  Just {before, after} -> Str.toLower before <> after
  Nothing -> Str.toLower str

uppercaseFirstChar :: String -> String
uppercaseFirstChar str = case Str.splitAt 1 str of
  Just {before, after} -> Str.toUpper before <> after
  Nothing -> Str.toLower str

startsWithUpperCase :: String -> Boolean
startsWithUpperCase s = case Regex.regex "^[A-Z]" RegexFlags.noFlags of
  Left _ -> false
  Right matcher -> Regex.test matcher s

stripTagFromId :: String -> String -> String
stripTagFromId "version" opId = opId
stripTagFromId tag opId =
  Str.replace (Str.Pattern $ snakeCaseToPascalCase tag) (Str.Replacement "") opId

stripModuleFromId :: ApiModuleName -> String -> String
stripModuleFromId modName opId =
  Str.replace (Str.Pattern $ NonEmptyList.last modName) (Str.Replacement "") opId
stripModuleFromId _ opId = opId

snakeCaseToPascalCase :: String -> String
snakeCaseToPascalCase =
  Str.split (Str.Pattern "_")
  >>> map uppercaseFirstChar
  >>> Str.joinWith ""