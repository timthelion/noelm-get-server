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
import qualified Noelm.Internal.Dependencies as Deps
import Noelm.Internal.Dependencies
 (Deps
 ,name
 ,version)
import qualified Noelm.Internal.Documentation as Docs
import Noelm.Internal.Documentation
  (Document
  ,structure
  ,moduleName
  ,Entry
  ,name
  ,entries
  ,Content(Markdown,Value)
  ,raw
  ,assocPrec
  ,comment)
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

filterSpecialChars :: String -> String
filterSpecialChars s =
 case s of
  (c:cs) ->
   case c of
    '<' -> "&lt;" ++ filterSpecialChars cs
    '&' -> "&amp;" ++ filterSpecialChars cs
    _   -> c : filterSpecialChars cs
  [] -> []

depsToNoelm :: Deps -> String
depsToNoelm deps = 
    concat  [ "import Website.Skeleton (skeleton)\n"
            , "import Website.ColorScheme as C\n"
            , ""
            , "links = [ (\""
            , filterSpecialChars $ toLink deps ""
            , "\", toText \""
            , filterSpecialChars $ N.project (Deps.name deps)
            , "\") ]\n"
            , "\n"
            , "main = skeleton links scene (constant ())\n"
            , "\n"
            , "scene term () w =\n"
            , "  flow down\n"
            , "  [ color C.mediumGrey <| spacer w 1\n"
            , "  , width w [markdown|"
            , filterSpecialChars $ Deps.description deps
            , "|]\n"
            , "  , width w [markdown|"
            , concatMap markdownLink (Deps.exposed deps)
            , "|]\n"
            , "  , width w [markdown|The [source code is on GitHub]("
            , filterSpecialChars $ Deps.repo deps
            , "),\n"
            , "so you can star projects, report issues, and follow great library designers.\n"
            , "\n"
            , "See all previous versions of this library [here](/catalog/"
            , filterSpecialChars $ N.toFilePath (Deps.name deps)
            , ").|]\n"
            , "  ]\n"
            ]
    where
      markdownLink m =
        concat
         [ "["
         , filterSpecialChars m
         , "]("
         , filterSpecialChars $ toLink deps m
         , ")<br/>"]


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
           concat  [ "import Website.Skeleton (skeleton)\n"
                   , "import Website.ColorScheme as C\n"
                   , "import Docs (entry)\n"
                   , "import String\n"
                   , "\n"
                   , "links = [ (\""
                   , filterSpecialChars $ toLink deps ""
                   , "\", toText \""
                   , filterSpecialChars $ N.project (Deps.name deps)
                   , "\")\n"
                   , "        , (\""
                   , filterSpecialChars $ toLink deps name
                   , "\", toText \""
                   , filterSpecialChars name
                   , "\") ]\n"
                   , "\n"
                   , "main = skeleton links scene (constant ())\n"
                   , "\n"
                   , "scene term () w =\n"
                   , "    flow down . map snd . filter (String.contains (String.toLower term) . fst) <|\n"
                   , "    [ (\"\", color C.mediumGrey <| spacer w 1)\n"
                   , "    , "
                   , List.intercalate "\n    , " code
                   , "    ]\n"
                   ]
       (missing, _) ->
           Left $
            concat
             [ "In module "
             , filterSpecialChars name
             , ", could not find documentation for: "
             , filterSpecialChars $ List.intercalate ", " missing]
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
      Markdown md ->
        Right $ concat
         [ "(,) \"\" <| width w [markdown|<style>h1,h2,h3,h4 {font-weight:normal;} h1 {font-size:20px;} h2 {font-size:18px;}</style>"
         , filterSpecialChars md
         , "|]" ]
      Value name ->
          case Map.lookup name entries of
            Nothing -> Left name
            Just entry ->
                Right $ unwords
                 [ "(,)"
                 , "(String.toLower \""
                 , filterSpecialChars name
                 , "\")"
                 , "<|"
                 , "entry w"
                 , "\""
                 , filterSpecialChars name
                 , "\" \""
                 , filterSpecialChars $ raw entry
                 , "\" "                   
                 , filterSpecialChars $ 
                    case assocPrec entry of
                      Nothing -> "Nothing"
                      Just ap -> "(" ++ show (Just ap) ++ ")"
                 , "[markdown|"
                 , filterSpecialChars $ comment entry
                 , "|]"
                 ]
