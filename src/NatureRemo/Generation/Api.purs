module NatureRemo.Generation.Api where

import Prelude

import Data.Array as Array
import Data.List (List(..))
import Data.List as List
import Data.StrMap (StrMap)
import Data.StrMap as StrMap
import Data.Tuple (Tuple(..))
import Data.Swagger.AST as AST
import NatureRemo.Generation.Module (generateEndpointModules)
import NatureRemo.Generation.Definitions (NatureRemoSchema, generateDefinitionModules)
import Data.Swagger.JsonSchema (Schema)
import Data.Swagger (Swagger)
import Partial.Unsafe (unsafePartial)

generateApi :: Partial => AST.ApiModuleName -> Swagger -> AST.ApiAst
generateApi moduleNs swagger = mergeModules endpointModules definitionModules
  where
    endpointModules = generateEndpointModules moduleNs swagger.paths
    definitionModules = mkDefinitionsAst swagger.definitions

    mkDefinitionsAst :: StrMap Schema -> Array AST.ApiModule
    mkDefinitionsAst schemas = schemas
      # parseSchemas
      # \s -> unsafePartial (generateDefinitionModules moduleNs s)

    parseSchemas :: StrMap Schema -> Array NatureRemoSchema
    parseSchemas = StrMap.toUnfoldable
      >>> map (\(Tuple name schema) -> {name, schema})

mergeModules :: Array AST.ApiModule -> Array AST.ApiModule -> AST.ApiAst
mergeModules endpointsModules defsModules =
  { modules: Array.fromFoldable $
      mergeAsts' (List.fromFoldable endpointsModules) (List.fromFoldable defsModules) Nil }

  where
    mergeAsts' :: List AST.ApiModule -> List AST.ApiModule -> List AST.ApiModule -> List AST.ApiModule
    mergeAsts' Nil Nil merged = merged
    mergeAsts' Nil defs merged = merged <> defs
    mergeAsts' endpoints Nil merged = merged <> endpoints
    mergeAsts' (Cons endpointMod restEndpoints) defs merged =
      case List.partition (\{name} -> name == endpointMod.name) defs of
        {yes: Cons matchingDef Nil, no: otherDefs} ->
          mergeAsts' restEndpoints otherDefs (Cons (mergeTwoModules endpointMod matchingDef) merged)
        {yes: _, no: otherDefs} -> mergeAsts' restEndpoints otherDefs (Cons endpointMod merged)

mergeTwoModules :: AST.ApiModule -> AST.ApiModule -> AST.ApiModule
mergeTwoModules {name, imports: endpointsImports, declarations: endpointsDecls}
             {imports: defsImports, declarations: defsDecls} =
  {name, imports, declarations}
  where
    imports = Array.nub $ Array.sort $ endpointsImports <> defsImports
    declarations = endpointsDecls <> defsDecls