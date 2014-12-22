{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module HappStack.Util.PageUtil where

import System.IO.Unsafe

import Network.Socket hiding (send, sendTo, recv, recvFrom)
import Network.Socket.ByteString (send, recv)
import qualified Data.ByteString.Char8 as B8

import qualified Data.Text as T

import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import Happstack.Server

import Text.Blaze.Internal (stringValue,preEscapedString)

import Couch.CouchUtil

import Control.Monad.Except as EX

unsafePrint :: (Show s) => s -> ()
unsafePrint s = unsafePerformIO (print s)

dropFirst :: a -> b -> b
dropFirst !x !y = y

unsafePrint' :: (Show s) => s -> a -> a
unsafePrint' x y = dropFirst (unsafePrint x) y

netPrint :: String -> Int -> String -> IO ()
netPrint host port msg = withSocketsDo $ do
    addrInfo <- getAddrInfo Nothing (Just host) (Just $ show port)
    let serverAddr = head addrInfo
    sock <- socket (addrFamily serverAddr) Stream defaultProtocol
    connect sock (addrAddress serverAddr)
    msgSender msg sock
    sClose sock
 
unsafeNetPrint :: String -> Int -> String -> a -> a
unsafeNetPrint !host !port !msg y = dropFirst (netPrint host port msg) (y)

msgSender :: String -> Socket -> IO ()
msgSender m sock = do
  let msg = B8.pack m
  send sock $ msg
  return ()

startEditor :: String -> H.Html
startEditor htmlId =
    H.toHtml $
      htmlId ++ " = ace.edit('" ++ htmlId ++ "') ;\n" ++
      htmlId ++ ".setTheme('ace/theme/monokai') ;\n" ++
      htmlId ++ ".getSession().setMode('ace/mode/javascript') ;\n"

setEditorNoSoftTabs :: String -> H.Html
setEditorNoSoftTabs name =
    H.toHtml $ name ++ ".getSession().setUseSoftTabs(false) ;\n"

setEditorMode :: String -> String -> H.Html
setEditorMode name mode =
    H.toHtml $ name ++ ".getSession().setMode('ace/mode/" ++ mode ++ "') ;\n"

setToVimEditor :: String -> H.Html
setToVimEditor name = H.toHtml $ name ++ ".setKeyboardHandler('ace/keyboard/vim') ;\n"

setToVim_string :: String -> String
setToVim_string name = name ++ ".setKeyboardHandler('ace/keyboard/vim') ;\n"

setEditorWrap :: String -> H.Html
setEditorWrap name =
    H.toHtml $ name ++ ".getSession().setUseWrapMode(true) ;\n"

setShowInvisible :: String -> H.Html
setShowInvisible name =
    H.toHtml $ name ++ ".setShowInvisibles(true) ;\n"

atLoad :: H.Html -> H.Html
atLoad body = do
    H.toHtml ("$(function(){\n"::String)
    body
    H.toHtml ("\n});"::String)

element :: String -> H.Html -> H.Html
element id body = do
    H.toHtml ("$('#" ++ id ++ "')"::String)
    body

setEditorSizeToScreen :: String -> H.Html
setEditorSizeToScreen name =
    atLoad $ do
        element name $ do
            H.toHtml (".css('height', window.innerHeight*0.8 + 'px' ) ;\n"::String)

rawTemplate :: T.Text -> H.Html -> H.Html -> H.Html -> Response
rawTemplate title header footer body = toResponse $
    H.html $ do
        H.head $ do
            H.title (H.toHtml title)
            H.link H.! A.rel "stylesheet" H.! A.type_ "text/css" H.! A.href "/files/css.css"
            H.link H.! A.rel "stylesheet" H.! A.type_ "text/css" H.! A.href "/files/Page1.css"
            H.script H.! A.src "/files/ace-builds/src-noconflict/ace.js" H.! A.type_ "text/javascript" H.! A.charset "utf-8" $ ""
            H.script H.! A.src "/files/jquery-2.0.3.min.js" H.! A.type_ "text/javascript" $ ""
        H.body $ do
            header
            body
            footer

empty :: H.Html
empty = H.toHtml (""::String)

template :: T.Text -> H.Html -> Response
template title body = rawTemplate title header footer body
    where
      header = do
          empty
      footer = do
          empty

template1 :: T.Text -> T.Text -> H.Html -> Response
template1 title head body = rawTemplate title (H.h1 $ H.toHtml head) empty body

doRedirect url = seeOther (url :: String) (toResponse ())

button :: String -> String -> H.Html -> H.Html
button url cssClass body =
    H.a H.! A.class_ (stringValue $ "link " ++ cssClass) H.! A.href (stringValue url) $ do
        H.div H.! A.style "display:inline-block;" H.! A.class_ "button" $ body

dumbButton :: String -> H.Html -> H.Html
dumbButton cssClass body =
    H.div H.! A.class_ (stringValue $ "link " ++ cssClass) $ do
        H.div H.! A.style "display:inline-block;" H.! A.class_ "button" $ body

hasDot :: String -> Bool
hasDot "" = False
hasDot (x:[]) = x == '.'
hasDot (x:xs) = (x == '.') || (hasDot xs)

rawReturn :: String -> ServerPartT IO Response
rawReturn x =
    ok $ toResponse $ do
        (preEscapedString x)

okOrError :: Either String String -> ServerPartT IO Response
okOrError t =
    case t of
        Right a -> rawReturn a
        Left a  -> rawReturn a

returnActionOrError :: ExceptT String IO String -> ServerPartT IO Response
returnActionOrError action = do
    c <- liftIO $ runExceptT action
    okOrError c

toOk :: a -> ExceptT String IO String
toOk _ = return "ok"
