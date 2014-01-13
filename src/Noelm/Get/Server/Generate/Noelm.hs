{-# LANGUAGE OverloadedStrings #-}
module Noelm.Get.Server.Generate.Noelm (generate) where

{- external modules -}
import Control.Monad.Error
import Control.Applicative ((<$>),(<*>))
import qualified Data.Aeson as Json
import qualified Data.Map as Map
import qualified Data.List as List
import qualified Data.Char as Char
import qualified Data.Maybe as Maybe
import qualified Data.Either as Either
import qualified Data.ByteString.Lazy.Char8 as BS
import qualified System.FilePath as FP
import qualified System.Directory as Dir
import System.FilePath ((</>), (<.>))
import Text.Parsec

{- Noelm moduels -}
import Noelm.Internal.Dependencies as Deps
import Noelm.Internal.Documentation as Docs
import qualified Noelm.Internal.Name as N

{- noelm-get modules -}
import qualified Noelm.Get.Utils.Paths as Path

generate :: [Document] -> Deps -> FilePath -> ErrorT String IO [FilePath]
generate docs deps directory = (++) <$> makeDocs <*> makeDeps
    where
      makeDocs =
          either throwError (liftIO . mapM (writeDocs directory)) (mapM (docToNoelm deps) docs)

      makeDeps =
        do liftIO $ writeFile (directory </> Path.index) (depsToNoelm deps)
           return [directory </> Path.index]

depsToNoelm :: Deps -> String
depsToNoelm deps = 
    unlines [ "import Website.Skeleton (skeleton)"
            , "import Website.ColorScheme as C"
            , ""
            , "links = [ (\"" ++ toLink deps "" ++ "\", toText \"" ++ N.project (Deps.name deps) ++ "\") ]"
            , ""
            , "main = skeleton links scene (constant ())"
            , ""
            , "scene term () w ="
            , "  flow down"
            , "  [ color C.mediumGrey <| spacer w 1"
            , "  , width w [markdown|" ++ Deps.description deps ++ "|]"
            , "  , width w [markdown|" ++ concatMap markdownLink (Deps.exposed deps) ++ "|]"
            , "  , width w [markdown|The [source code is on GitHub](" ++ Deps.repo deps ++ "),"
            , "so you can star projects, report issues, and follow great library designers."
            , ""
            , "See all previous versions of this library [here](/catalog/" ++ N.toFilePath (Deps.name deps) ++ ").|]"
            , "  ]"
            ]
    where
      markdownLink m = "[" ++ m ++ "](" ++ toLink deps m ++ ")<br/>"


toLink deps m =
    concat [ "/catalog/"
           , N.toFilePath (Deps.name deps)
           , "/", show (Deps.version deps), "/"
           , map (\c -> if c == '.' then '-' else c) m
           ]

writeDocs :: FilePath -> (String,String) -> IO FilePath
writeDocs directory (name, code) =
  do writeFile fileName code
     return fileName
  where
    fileName = directory </> map (\c -> if c == '.' then '-' else c) name <.> "noelm"

docToNoelm :: Deps -> Document -> Either String (String,String)
docToNoelm deps doc =
  do contents <- either (Left . show) Right $ parse (parseDoc []) name (structure doc)
     case Either.partitionEithers $ map (contentToNoelm (getEntries doc)) contents of
       ([], code) ->
           Right . (,) name $
           unlines [ "import Website.Skeleton (skeleton)"
                   , "import Website.ColorScheme as C"
                   , "import Docs (entry)"
                   , "import String"
                   , ""
                   , "links = [ (\"" ++ toLink deps "" ++ "\", toText \"" ++ N.project (Deps.name deps) ++ "\")"
                   , "        , (\"" ++ toLink deps name ++ "\", toText " ++ show name ++ ") ]"
                   , ""
                   , "main = skeleton links scene (constant ())"
                   , ""
                   , "scene term () w ="
                   , "    flow down . map snd . filter (String.contains (String.toLower term) . fst) <|"
                   , "    [ (\"\", color C.mediumGrey <| spacer w 1)"
                   , "    , " ++ List.intercalate "\n    , " code
                   , "    ]"
                   ]
       (missing, _) ->
           Left $ "In module " ++ name ++ ", could not find documentation for: " ++ List.intercalate ", " missing
  where
    name = moduleName doc
    entries = getEntries doc

    parseDoc contents =
        choice [ eof >> return contents
               , do try (string "@docs")
                    whitespace
                    values <- sepBy1 (var <|> op) comma
                    parseDoc (contents ++ map Value values)
               , do let stop = eof <|> try (string "@docs" >> return ())
                    md <- manyTill anyChar (lookAhead stop)
                    parseDoc (contents ++ [Markdown md])
               ]

    var = (:) <$> letter <*> many (alphaNum <|> oneOf "_'")
    op = do char '(' >> whitespace
            operator <- many1 (satisfy Char.isSymbol <|> oneOf "+-/*=.$<>:&|^?%#@~!")
            whitespace >> char ')'
            return operator

    comma = try (whitespace >> char ',') >> whitespace
    whitespace = many (satisfy (`elem` " \n\r"))

getEntries :: Document -> Map.Map String Entry
getEntries doc =
    Map.fromList $ map (\entry -> (Docs.name entry, entry)) (entries doc)

contentToNoelm :: Map.Map String Entry -> Content -> Either String String
contentToNoelm entries content =
    case content of
      Markdown md -> Right $ "(,) \"\" <| width w [markdown|<style>h1,h2,h3,h4 {font-weight:normal;} h1 {font-size:20px;} h2 {font-size:18px;}</style>" ++ md ++ "|]"
      Value name ->
          case Map.lookup name entries of
            Nothing -> Left name
            Just entry ->
                Right $ unwords [ "(,)"
                                , "(String.toLower " ++ show name ++ ")"
                                , "<|"
                                , "entry w"
                                , show name
                                , show (raw entry)
                                , case assocPrec entry of
                                    Nothing -> "Nothing"
                                    Just ap -> "(" ++ show (Just ap) ++ ")"
                                , "[markdown|" ++ comment entry ++ "|]"
                                ]
