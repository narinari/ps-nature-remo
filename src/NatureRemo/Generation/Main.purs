module NatureRemo.Generation.Main where

import Control.Monad.Aff
import Data.Array as Array
import Data.List.NonEmpty (NonEmptyList(..))
import Data.List.NonEmpty as NonEmptyList
import Data.List as List
import Data.Traversable (sequence)
import Node.Encoding
import Node.FS
import Node.FS.Aff
import Partial.Unsafe
import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Aff.Console (CONSOLE, log)
import Control.Monad.Eff.Exception as Exception
import Data.Either
import Data.NonEmpty ((:|))
import Data.String as String
import Data.Swagger (Swagger)
import Data.Swagger.AST (ApiModule, ApiModuleName)
import NatureRemo.Generation.Api
import NatureRemo.Generation.Emitter as Emit
import Simple.JSON (class ReadForeign, readJSON)

generationConfig :: Config
generationConfig =
  { moduleNs: NonEmptyList ("NatureRemo" :| pure "Api")
  , outputDir: "src"
  , swaggerFile: "./definitions/swagger.json" }

type Config =
  { moduleNs :: ApiModuleName
  , outputDir :: String
  , swaggerFile :: String
  }

generateAndEmitApi :: forall e. Config -> Aff (console :: CONSOLE, fs :: FS | e) Unit
generateAndEmitApi {moduleNs, outputDir, swaggerFile} = do
  (swagger :: Swagger) <- loadSimpleJsonFile swaggerFile
  let apiAst = (unsafePartial $ generateApi moduleNs swagger)
  ensureDirsOnPathExist (NonEmptyList.cons outputDir moduleNs)
  _ <- unsafePartial $ sequence $ writeModuleFile outputDir <$> apiAst.modules
  log $ "Generated " <> show (Array.length apiAst.modules) <> " modules"
  pure unit

loadSimpleJsonFile :: forall a e. ReadForeign a => String -> Aff (fs :: FS | e) a
loadSimpleJsonFile path = do
  fileStr <- readTextFile UTF8 path
  unwrapEitherIntoAff $ readJSON fileStr

unwrapEitherIntoAff :: forall a b e. Show a => Either a b -> Aff e b
unwrapEitherIntoAff (Left error) = throwError (Exception.error $ show error)
unwrapEitherIntoAff (Right val) = pure val

ensureDirsOnPathExist :: forall e. NonEmptyList String -> Aff (console :: CONSOLE, fs :: FS | e) Unit
ensureDirsOnPathExist modulePath = do
  _ <- sequence $ ensureDirExists <$> outputDirs
  pure unit
  where
    outputDirs = mkOutputDir <$> (Array.range 1 $ NonEmptyList.length modulePath)
    mkOutputDir i = String.joinWith "/" $ List.toUnfoldable $ NonEmptyList.take i modulePath

ensureDirExists :: forall e. String -> Aff (console :: CONSOLE, fs :: FS | e) Unit
ensureDirExists outputDir = do
  dirResult <- try (mkdir outputDir)
  case dirResult of
    Left error -> pure unit
    Right ok -> log $ "Created output directory '" <> outputDir <> "'"

writeModuleFile :: forall e. Partial => String -> ApiModule -> Aff (console :: CONSOLE, fs :: FS | e) Unit
writeModuleFile dir mod@{name} = do
  let dropLast = NonEmptyList.init >>> NonEmptyList.fromList
  _ <- sequence $ ensureDirsOnPathExist <$> (dropLast (NonEmptyList.cons dir name))
  writeTextFile UTF8 fileName contents
  where
    fileName = dir <> "/" <> mkPath name <> ".purs"
    contents = Emit.emitApiModule mod
    mkPath = String.joinWith "/" <<< NonEmptyList.toUnfoldable

main :: forall e. Eff (console :: CONSOLE, fs :: FS | e) Unit
main = launchAff_ $ generateAndEmitApi generationConfig
