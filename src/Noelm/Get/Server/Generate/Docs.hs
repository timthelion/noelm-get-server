module Noelm.Get.Server.Generate.Docs where
{- external modules -}
import Control.Applicative
import Control.Monad.Error
import qualified Data.Aeson as Json
import qualified Data.ByteString.Lazy.Char8 as BS
import qualified Data.Map as Map
import System.FilePath
import System.IO
import System.Exit

{- Noelm modules -}
import qualified Noelm.Internal.Dependencies as D
import qualified Noelm.Internal.Name as N
import qualified Noelm.Internal.Paths as NPath

{- noelm-get modules -}
import qualified Noelm.Get.Utils.Paths as Path

{- internal modules -}
import qualified Noelm.Get.Server.Generate.Noelm as Noelm
import qualified Noelm.Get.Server.Generate.Html as Html
import qualified Noelm.Get.Server.Generate.Listing as Listing

generate :: FilePath -> ErrorT String IO ()
generate directory =
  do deps <- makeHtml directory
     liftIO $ Listing.add deps

regenerate :: IO ()
regenerate =
  do listings <- Listing.readListings
     result <- runErrorT $ mapM makeHtml (concatMap getDirs (Map.elems listings))
     case result of
       Right _ -> return ()
       Left err ->
           do hPutStrLn stderr $ "Failure when regenerating documentation:\n" ++ err
              exitFailure
  where
    getDirs (Listing.Listing name _ vs) =
        map (\version -> Path.libDir </> N.toFilePath name </> show version) vs

makeHtml :: FilePath -> ErrorT String IO D.Deps
makeHtml directory =
  do docs' <- liftIO $ BS.readFile $ directory </> Path.json
     deps' <- liftIO $ BS.readFile $ directory </> NPath.dependencyFile
     case (,) <$> Json.eitherDecode docs' <*> Json.eitherDecode deps' of
       Left err -> throwError err
       Right (docs,deps) ->
           do noelms <- Noelm.generate docs deps directory
              mapM_ Html.generatePublic noelms
              return deps
