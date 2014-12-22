{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Couch.PutOk where

import Data.Aeson
import GHC.Generics
import Network.HTTP.Conduit
import Data.Text (Text)
import qualified Data.Text as T

import Data.ByteString.Lazy
import Data.ByteString.Lazy.Char8 as C

import Data.CaseInsensitive ( CI )

--{"ok":true,"id":"3570ae7a22e4023f2bc0f184ec005a3a","rev":"1-dae30144fd5c2791c97f0bf66e231089"}

data PutOk = PutOk { id  :: !Text
                   , rev :: !Text
                   , ok   :: Bool
                    } deriving Generic
instance FromJSON PutOk
instance ToJSON PutOk
