{-# LANGUAGE DeriveGeneric #-}

import Data.Aeson
import GHC.Generics

data Foo = Foo { id :: Int, content :: String } deriving (Show, Generic)

instance FromJSON Foo
instance ToJSON Foo

