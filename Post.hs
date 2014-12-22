{-# LANGUAGE OverloadedStrings, DeriveGeneric, LambdaCase #-}

module Post where

import Prelude hiding (id)

import Network.HTTP.Base (urlEncode)

import Data.Aeson

import qualified GHC.Generics as G

import qualified Data.ByteString.Char8 as BS

import qualified Crypto.Hash.SHA256 as CY

import qualified HappStack.Util.PageUtil as PU
import qualified Couch.CouchUtil as CU

import Control.Monad.Except as EX

import Data.Text.Encoding

import Crypto.PasswordStore

import Data.Text (Text,pack,append)
import Data.Text.Lazy (unpack)
import qualified Data.Text as T

import Data.Maybe

import Data.Char

bdName = "newarchd"

data Post = Post { _id :: !(Maybe Text)
                 , _rev :: !(Maybe Text)
                 , content :: !Text
                 , user :: !Text
                 , parent :: !Text
                 , date :: !Text
                  } deriving G.Generic
instance FromJSON Post
instance ToJSON Post where
    toJSON post = object .
        ( \x -> case _rev post of
                     Just a  -> ( "_rev" .= a ):x
                     Nothing -> x
        ) .
        ( \x -> case _id post of
                     Just a  -> ( "_id" .= a ):x
                     Nothing -> x
        )
        $ [ "content"  .= content post
          , "user"     .= user    post
          , "parent"   .= parent  post
          , "date"     .= date    post
          ]

newPost :: String -> String -> String -> String -> Post
newPost pDate pUser pParent pContent =
    Post { content = T.pack pContent
         , user    = T.pack pUser
         , parent  = T.pack pParent
         , date    = T.pack pDate
         , _rev    = Nothing
         , _id     = Just $ T.pack $ mkPostId pDate pParent pUser pContent
          }

mkPostId :: String -> String -> String -> String -> String
mkPostId s1 s2 s3 s4 = s1 ++ s3 ++ cleaned
  where
    hashed = BS.unpack $ CY.hash $ BS.pack (s2 ++ s4)
    cleaned = concat $ map (\x -> if isAlphaNum x then (if x == ' ' then "S" else [x]) else show $ ord x) $ show hashed

justId :: Post -> Text
justId p = fromJust $ _id p

justIdString :: Post -> String
justIdString p = T.unpack $ justId p

justRev :: Post -> Text
justRev p = fromJust $ _rev p

justRevString :: Post -> String
justRevString p = T.unpack $ justRev p

getPost :: String -> ExceptT String IO Post
getPost id = do
    mPost <- liftIO ( CU.couchGet url :: IO (Maybe Post) )
    case mPost of
        Just post -> return post
        Nothing   -> throwError $ "Can't find post with id :" ++ id
  where
    url = bdName ++ "/" ++ (urlEncode id)

getChildrens :: String -> ExceptT String IO [Text]
getChildrens id = do
    mL <- liftIO ( CU.couchGet url :: IO (Maybe CU.DocList) )
    case mL of
        Just posts -> return $ map CU.id (CU.rows posts)
        Nothing    -> throwError $ "Can't find childrens of : " ++ id
  where
    url = bdName ++ "/_design/parent/_view/parent?key=\"" ++ (urlEncode id) ++ "\""

getRoots :: ExceptT String IO [Text]
getRoots = do
    mL <- liftIO ( CU.couchGet url :: IO (Maybe CU.DocList) )
    case mL of
        Just posts -> return $ map CU.id (CU.rows posts)
        Nothing    -> throwError $ "Can't find roots :/"
  where
    url = bdName ++ "/_design/roots/_view/roots"

getPosts :: ExceptT String IO [Post]
getPosts = do
    r <- liftIO $ CU.couchGet "newarchd/_design/posts/_view/posts" >>= \case
        Nothing -> return [] -- here I am, hiding an ugly error.
        Just l  -> return $ CU.rows l
    
    -- This shit is crazy, if ony of the posts can't be found the error eats everything
    dl <- forM r $ \x -> do
        getPost $ T.unpack $ CU.id x
    
    return dl
