{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Aeson as AE

import Control.Monad.IO.Class

import Data.Time.Clock.POSIX (getPOSIXTime)

import Control.Monad
import Control.Concurrent

import Network.HTTP.Base (urlEncode)

import Happstack.Server
import Happstack.Server.SimpleHTTPS
import Happstack.Server (nullConf, simpleHTTP, toResponse, ok, Conf)
import Happstack.Server.Internal.Types

import Happstack.Server

import Control.Applicative ((<$>), optional)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text.Lazy (unpack,unpack)
import Data.Char
import Text.Blaze.Html5 (Html, (!), a, form, input, p, toHtml, label)
import Text.Blaze.Html5.Attributes (action, enctype, href, name, size, type_, value)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy.Char8 as BC

import qualified Data.Text as T

import System.Exit

import Data.List.Split
import Data.List

import qualified HappStack.Util.PageUtil as PU
import qualified Couch.CouchUtil as CU
import qualified Couch.PutOk as CPU

import qualified Post as P

import qualified Control.Monad.Except as EX

import qualified Network.HTTP.Conduit as HC

serverConfiguration :: Conf
serverConfiguration = Conf
    {
        port     	 = 9000 ,
        validator	 = Nothing ,
        logAccess	 = Just logMAccess ,
        timeout  	 = 30 ,
        threadGroup	 = Nothing
    }

urlToModuleName :: String -> String
urlToModuleName url =
    concat .
    intersperse "." .
    map (\(x:xs)-> toUpper x : xs ) .
    splitOn "/" .
    head .
    splitOn "?" .
    tail
    $ url

homePage :: String -> ServerPartT IO Response
homePage completeUrl = do
    
    action <- looks "action"
    
    case action of
        ["getPosts"]     -> getPosts
        ["getPost"]      -> getPost
        ["writePost"]    -> writePost
        ["deletePost"]   -> deletePost
        ["getRoots"]     -> getRoots
        ["getChildrens"] -> getChildrens
        ["echo"]         -> look "content" >>= PU.rawReturn
        _                -> PU.rawReturn "Nothing to do"

deletePost :: ServerPartT IO Response
deletePost = do
    id <- look "id"
    
    mPost <- liftIO $ EX.runExceptT $ P.getPost id
    
    case mPost of
        Left  e  -> PU.rawReturn "{}"
        Right p  -> do
            _ <- liftIO $ CU.couchDelete (P.bdName ++ "/" ++ ( urlEncode $ P.justIdString p )) (P.justRevString p)
            PU.rawReturn "{}"
    

writePost :: ServerPartT IO Response
writePost = do
    
    content <- look "content"
    parent  <- look "parent"
    user    <- look "user"
    
    timeStamp <- liftIO $ fmap show getPOSIXTime
    hcr <- liftIO $ CU.couchPut P.bdName (AE.encode $ P.newPost timeStamp user parent content)
    
    let cr = HC.responseBody hcr
    
    let mOk = AE.decode cr :: Maybe CPU.PutOk
    
    PU.rawReturn $ case mOk of
        Just a  -> "{ \"id\": \"" ++ (T.unpack $ CPU.id a) ++ "\"}"
        Nothing -> "Faild to wrtie post (maybe?)"

getChildrens :: ServerPartT IO Response
getChildrens = do
    
    postId <- look "id"
    
    ret <- liftIO $ EX.runExceptT $ P.getChildrens postId
    
    PU.rawReturn $ case ret of
        Left e  -> e
        Right e -> BC.unpack $ AE.encode e

getRoots :: ServerPartT IO Response
getRoots = do
    
    ret <- liftIO $ EX.runExceptT $ P.getRoots
    
    PU.rawReturn $ case ret of
        Left e  -> e
        Right e -> BC.unpack $ AE.encode e

getPosts :: ServerPartT IO Response
getPosts = do
    ret <- liftIO $ EX.runExceptT $ P.getPosts
    PU.rawReturn $ case ret of
        Left e  -> e
        Right e -> BC.unpack $ AE.encode e

getPost :: ServerPartT IO Response
getPost = do
    postId <- look "id"
    
    mPost <- liftIO $ EX.runExceptT $ P.getPost postId
    
    PU.rawReturn $ case mPost of
        Left e     -> e
        Right post -> BC.unpack $ AE.encode post

serv :: IO ()
serv = simpleHTTP serverConfiguration $ msum
	[
		dir "files" $ serveDirectory EnableBrowsing [] "./files" ,
		uriRest $ homePage
	]

exitCommand :: String -> Bool
exitCommand x = x == "exit" || x == "q"

inputs :: IO ()
inputs = do
    l <- getLine
    when (exitCommand l) ( exitSuccess )
    putStrLn ("ok" ++ l)

main = do
    _ <- forkIO serv
    forever inputs
