{-# LANGUAGE OverloadedStrings, DeriveDataTypeable #-}
module Noelm.Get.Server.Generate.Html where

import Control.Monad.Error
import Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Text.Blaze.Html.Renderer.String (renderHtml)
import System.FilePath as FP
import System.Directory
import qualified Data.ByteString.Char8 as BSC

import qualified Noelm.Get.Utils.Commands as Cmd

generatePublic :: FilePath -> ErrorT String IO ()
generatePublic path =
  do Cmd.run "noelm" ["--make","--runtime=/resources/noelm-runtime.js"
                   , "--build-dir=.", "--src-dir=src", path]
     liftIO $ removeFile path
     liftIO $ adjustHtmlFile $ FP.replaceExtension path "html"

generateSrc :: FilePath -> ErrorT String IO ()
generateSrc path =
  do Cmd.run "noelm" ["--make","--runtime=/resources/noelm-runtime.js"
                   , "--build-dir=.", "--src-dir=src", path]
     let old = FP.replaceExtension path "html"
         new = FP.replaceDirectory old "public"
     liftIO $ do
       renameFile old new
       adjustHtmlFile new

adjustHtmlFile :: FilePath -> IO ()
adjustHtmlFile file =
  do src <- BSC.readFile file
     let (before, after) = BSC.breakSubstring "<title>" src
     BSC.writeFile file $ BSC.concat [before, style, after]

style :: BSC.ByteString
style = 
    "<style type=\"text/css\">\n\
    \  a:link {text-decoration: none; color: rgb(15,102,230);}\n\
    \  a:visited {text-decoration: none; color: rgb(15,102,230);}\n\
    \  a:active {text-decoration: none}\n\
    \  a:hover {text-decoration: underline; color: rgb(234,21,122);}\n\
    \  body { font-family: \"Lucida Grande\",\"Trebuchet MS\",\"Bitstream Vera Sans\",Verdana,Helvetica,sans-serif !important; }\n\
    \  p, li { font-size: 14px !important;\n\
    \          line-height: 1.5em !important; }\n\
    \</style>"