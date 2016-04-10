{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DefaultSignatures          #-}
{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

import qualified Data.ByteString.Lazy.Char8 as L
import Happstack.Server
import Happstack.Server.Types

import Control.Monad
import Control.Monad.Logger (runStderrLoggingT)
import Control.Monad.IO.Class (liftIO)

import Data.Aeson as A
import Data.Data (Data, Typeable)
import qualified Data.HashMap as HM
import Data.Maybe
import Data.String

import Database.Persist
import Database.Persist.Sqlite
import Database.Persist.TH

import GHC.Generics

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
User json
  soundCloudId String
  deriving Generic Show Eq Data Typeable

Playlist json
  songs [Song]
  collaborators [User]
  deriving Generic Show Eq Data Typeable

Song json
  name String
  url String
  deriving Generic Show Eq Data Typeable
|]

-- put this function in a library somewhere
getBody :: ServerPart L.ByteString
getBody = do
  req  <- askRq 
  body <- liftIO $ takeRequestBody req 
  case body of 
    Just rqbody -> return . unBody $ rqbody
    Nothing     -> return ""

main :: IO ()
main = do
  migrateDB
  startServer

migrateDB :: IO ()
migrateDB = runSqlite "db" $ do
  runMigration migrateAll

startServer :: IO ()
startServer = simpleHTTP nullConf $ msum routes

routes :: [ServerPartT IO Response]
routes = 
  [
    -- dir "playlists" playlists
    dir "users" usersHandler
  ]  

usersHandler :: ServerPart Response
usersHandler = msum
  [ method GET >> getUsers
  , method POST >> postUser
  ]

getUsers :: ServerPart Response
getUsers = do
  users <- fetchAllUsers
  ok $ toResponse $ A.encode users

postUser :: ServerPart Response
postUser = do
  body <- getBody -- it's a ByteString
  let user = fromJust $ A.decode body :: User -- how to parse json
  userKey <- saveUser user
  ok $ toResponse $ A.encode userKey -- how to send json back. 

fetchAllUsers :: ServerPart [(Entity User)]
fetchAllUsers = runSqlite "db" $ selectList [] []

playlists :: ServerPart Response
playlists = do
  body <- getBody -- it's a ByteString
  let user = fromJust $ A.decode body :: User -- how to parse json
  userId <- saveUser user
  ok $ toResponse $ A.encode user -- how to send json back. 
  -- ok $ toResponse $ fromSqlKey userId

saveUser :: User -> ServerPart (Key User)
saveUser user = runSqlite "db" $ do
  insert $ user
