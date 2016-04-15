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
  artist String
  duration Double
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

parseBody :: FromJSON a => ServerPart (Maybe a)
parseBody = do
  body <- getBody
  return . A.decode $ body

main :: IO ()
main = do
  migrateDB
  startServer

migrateDB :: IO ()
migrateDB = runSqlite "db" $ do
  runMigration migrateAll

startServer :: IO ()
startServer = simpleHTTP nullConf $ routes

routes :: ServerPartT IO Response
routes = msum
  [ dir "users" usersHandler
  , dir "playlists" playlistsHandler
  ]  

usersHandler :: ServerPart Response
usersHandler = msum
  [ method GET >> getUsers
  , method POST >> postUser
  ]

playlistsHandler :: ServerPart Response
playlistsHandler = msum
  [ method GET >> fetchPlaylistHandler
  , method POST >> playlistPostHandler
  ]

getUsers :: ServerPart Response
getUsers = do
  users <- fetchAllUsers
  ok $ toResponse $ A.encode users

postUser :: ServerPart Response
postUser = do
  user <- parseBody
  case user of
    Just user -> saveUser user >>= (ok . toResponse . A.encode)
    Nothing   -> badRequest . toResponse $ ("Could not parse request" :: String)

fetchAllUsers :: ServerPart [(Entity User)]
fetchAllUsers = runSqlite "db" $ selectList [] []

saveUser :: User -> ServerPart (Key User)
saveUser user = runSqlite "db" $ do
  insert $ user

fetchPlaylistHandler :: ServerPart Response
fetchPlaylistHandler = path $ fetchPlaylist

fetchPlaylist :: Int -> ServerPart Response
fetchPlaylist playlistId = do
  playlistEntity <- runSqlite "db" $ do
    let playlistKey = toSqlKey $ fromIntegral playlistId :: Key Playlist
    get playlistKey
  case playlistEntity of
    Just playlist -> ok $ toResponse $ A.encode $ playlistEntity
    Nothing       -> badRequest $ toResponse $ ("Playlist does not exist" :: String) 

playlistPostHandler :: ServerPart Response
playlistPostHandler = msum
  [ (path $ \playlistPath -> dir "tracks" $ addSongToPlaylistHandler (read playlistPath :: Int))
  , createPlaylistHandler
  ]

addSongToPlaylistHandler :: Int -> ServerPart Response
addSongToPlaylistHandler playlistId = do
  song <- fmap fromJust parseBody
  result <- addSongToPlaylist playlistId song
  case result of 
    Just _  -> ok $ toResponse $ ("Track added" :: String)
    Nothing -> badRequest $ toResponse ("Playlist does not exist" :: String) 
  -- case song of 
  --   Just song ->
  --     case addSongToPlaylist playlistId song of
  --       Just _  -> return $ ok $ toResponse $ ("Track added" :: String)
  --       Nothing -> return $ badRequest $ toResponse ("Playlist does not exist" :: String) 
  --   Nothing -> badRequest $ toResponse ("Playlist does not exist" :: String) 

createPlaylistHandler :: ServerPart Response
createPlaylistHandler = do
  playlist <- parseBody
  case playlist of 
    Just playlist -> do
      runSqlite "db" $ do
        insert $ (playlist :: Playlist)
      ok $ toResponse $ ("Playlist added" :: String)
    Nothing       -> badRequest $ toResponse ("Couldn't parse playlist" :: String) 

addSongToPlaylist :: Int -> Song -> ServerPart (Maybe ())
addSongToPlaylist playlistId song = runSqlite "db" $ do
    let playlistKey = toSqlKey $ fromIntegral playlistId :: Key Playlist
    playlist <- get playlistKey
    case playlist of
      Just playlist -> do
        let newSongs = song : (playlistSongs playlist)
        update playlistKey [PlaylistSongs =. newSongs]
        return $ Just ()
      Nothing       -> return $ Nothing

