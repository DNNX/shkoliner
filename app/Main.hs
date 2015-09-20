{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Lib

-- import Data.Foldable
-- import System.Environment

-- import Data.Aeson
-- import Data.Time.Calendar
-- import GHC.Generics
import Network.Wai
import Network.Wai.Handler.Warp
import Servant

type ShkoloAPI = "articles" :> Get '[JSON] [ArticleMetaData]

type WholeAPI = ShkoloAPI :<|> Raw

server :: Server ShkoloAPI
server = return []

shkoloAPI :: Proxy ShkoloAPI
shkoloAPI = Proxy

wholeAPI :: Proxy WholeAPI
wholeAPI = Proxy

app :: Application
app = serve wholeAPI (server :<|> serveDirectory "static")

main :: IO ()
main = run 8081 app
