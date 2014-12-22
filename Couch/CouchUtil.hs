{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Couch.CouchUtil where

import Data.Aeson
import GHC.Generics
import Network.HTTP.Conduit
import Data.Text (Text)
import qualified Data.Text as T

import Data.ByteString.Lazy
import Data.ByteString.Lazy.Char8 as C

import Data.CaseInsensitive ( CI )


couchUrl :: String
couchUrl = "http://0.0.0.0:5984/"


data DocRes = DocRes { id :: !Text
                     , key :: !Text
                     , value :: !Value
                      } deriving (Show, Generic)
instance FromJSON DocRes
instance ToJSON DocRes

data DocList = DocList { offset :: Int
                       , total_rows :: Int
                       , rows :: [DocRes]
                        } deriving (Show, Generic)
instance FromJSON DocList
instance ToJSON DocList

getAllDocs :: String -> IO (Maybe DocList)
getAllDocs base = do
    json <- simpleHttp $ couchUrl ++ base ++ "/_all_docs"
    return $ ((decode json)::(Maybe DocList))

couchGet :: FromJSON a => String -> IO (Maybe a)
couchGet query = do
    json <- simpleHttp $ couchUrl ++ query
    return $ (decode json)

emptyRev :: Text
emptyRev = "0-00000000000000000000000000000000"

couchPut :: String -> ByteString -> IO (Response ByteString)
couchPut url obj = do
    
    initReq <- parseUrl (couchUrl++url)
    let req = initReq
            { method = "POST"
            , requestBody = RequestBodyLBS obj
            , requestHeaders = [( ("Content-Type") ,"application/json")]
            }
    
    manager <- newManager conduitManagerSettings
    
    r <- httpLbs req manager
    
    return r

couchDelete :: String -> String -> IO (Response ByteString)
couchDelete url rev = do
    
    initReq <- parseUrl (couchUrl++url++"?rev="++rev)
    let req = initReq { method = "DELETE" }
    
    manager <- newManager conduitManagerSettings
    
    r <- httpLbs req manager
    
    return r
