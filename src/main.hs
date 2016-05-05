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

import Control.Applicative
import Control.Monad
import Control.Monad.Logger (runStderrLoggingT)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class
import Control.Monad.Trans.Maybe

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
  UniqueSoundCloudId soundCloudId
  deriving Generic Show Eq Data Typeable

Playlist json
  name String
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

parseBody :: FromJSON a => MaybeT (ServerPartT IO) a
parseBody = lift getBody >>= MaybeT . return . A.decode

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
getUsers = fetchAllUsers >>= (ok . toResponse . A.encode)

postUser :: ServerPart Response
postUser = do
  savedUser <- runMaybeT $ parseBody >>= saveUser
  case savedUser of
    Just user -> ok . toResponse . A.encode $ user
    Nothing   -> badRequest . toResponse $ ("Could not parse request" :: String)    

fetchAllUsers :: ServerPart [(Entity User)]
fetchAllUsers = runSqlite "db" $ selectList [] []

saveUser :: User -> MaybeT (ServerPartT IO) (Key User)
saveUser user = MaybeT $ runSqlite "db" $ insertUnique user

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
  , (path $ \playlistPath -> dir "collaborators" $ addSongToPlaylistHandler (read playlistPath :: Int))
  , createPlaylistHandler
  ]

addSongToPlaylistHandler :: Int -> ServerPart Response
addSongToPlaylistHandler playlistId = do
  result <- runMaybeT $ parseBody >>= addSongToPlaylist playlistId
  case result of 
    Just _  -> ok $ toResponse $ ("Track added" :: String)
    Nothing -> badRequest $ toResponse ("Playlist does not exist" :: String) 

addContributorToPlaylistHandler :: Int -> ServerPart Response
addContributorToPlaylistHandler playlistId = do
  result <- runMaybeT $ parseBody >>= addContributorToPlaylist playlistId
  case result of
    Just _  -> ok $ toResponse $ ("Contributor added" :: String)
    Nothing -> badRequest $ toResponse ("Playlist does not exist" :: String) 

addContributorToPlaylist :: Int -> User -> MaybeT (ServerPartT IO) ()
addContributorToPlaylist playlistId user = MaybeT . runSqlite "db" $ do
  let playlistKey = toSqlKey $ fromIntegral playlistId :: Key Playlist
  playlist <- get playlistKey
  case playlist of
    Just playlist -> do
      let newCollaborators = user : (playlistCollaborators playlist)
      update playlistKey [PlaylistCollaborators =. newCollaborators]
      return $ Just ()
    Nothing       -> return $ Nothing

createPlaylistHandler :: ServerPart Response
createPlaylistHandler = do
  playlist <- runMaybeT parseBody
  case playlist of 
    Just playlist -> do
      runSqlite "db" $ insert (playlist :: Playlist)
      ok $ toResponse ("Playlist added" :: String)
    Nothing       -> badRequest $ toResponse ("Couldn't parse playlist" :: String) 

addSongToPlaylist :: Int -> Song -> MaybeT (ServerPartT IO) ()
addSongToPlaylist playlistId song = MaybeT . runSqlite "db" $ do
    let playlistKey = toSqlKey $ fromIntegral playlistId :: Key Playlist
    playlist <- get playlistKey
    case playlist of
      Just playlist -> do
        let newSongs = song : (playlistSongs playlist)
        update playlistKey [PlaylistSongs =. newSongs]
        return $ Just ()
      Nothing       -> return $ Nothing

