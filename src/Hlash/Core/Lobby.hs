{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
module Hlash.Core.Lobby where

import qualified Data.Text as T
import System.Random (StdGen, next, getStdGen)
import Control.Monad.Loops
import qualified Data.HashMap.Strict as H
import qualified Data.ByteString.Lazy as B 
import GHC.Generics (Rep)
import Data.List (nub)
import Data.Aeson

import Hlash.App.Env
import Hlash.Effects.RoomCode
import Hlash.Core.Profile
import Hlash.Core.Game.Init
import Hlash.Core.Question
import Hlash.Core.Types
import Hlash.Db (WithDb)

mockProfile = Profile { username = "a"}
mockLobbies = createLobby'  testingCode mockProfile H.empty


createLobby' :: RoomCode -> Profile -> Lobbies  -> Lobbies
createLobby' code user lobbies   = H.insert code newLobby lobbies
  where
    -- newLobby = Lobby code user [user] [] 
    newLobby = Lobby code user [user] 

createLobby :: MonadIO m =>  Profile -> LobbyStore  -> m MsgOut
createLobby user lobbyStore = untilJust $ do
  candidate <- generateCode
  atomically $ do
    -- if the candidate key is already in use loop again, TODO come up with a better solution to this
    lobbies <- readTVar lobbyStore
    let keyExists = maybe  (Just candidate) (\_ -> Nothing) (H.lookup candidate lobbies) 
    case keyExists of
      Just _ -> Just <$> do modifyTVar' lobbyStore (newLobby candidate)
                            updatedLobbies <- readTVar lobbyStore
                            pure $ maybe (LobbyError NoLobby) (\lobby -> NewLobbyState lobby) (H.lookup candidate updatedLobbies)
      Nothing -> pure Nothing
    
      where newLobby code = createLobby' code user 

addProfileToLobby' ::  RoomCode -> Profile -> Lobbies -> Either LobbyError Lobbies
addProfileToLobby' code user lobbies =
  maybe (Left NoLobby) (\x -> updateMembers x) maybeLobby
  where
    updateMembers lobby = case inLobby lobby < maxCapacity of
      True -> Right $ H.insert code (updatedLobby lobby) lobbies
      False -> Left LobbyFull
    maybeLobby = H.lookup code lobbies 
    inLobby = length . lobbyMembers 
    updatedLobby lobby = lobby  {lobbyMembers = nub  $ user : lobbyMembers lobby}
  
addProfileToLobby :: MonadIO m => RoomCode -> Profile -> LobbyStore -> m MsgOut
addProfileToLobby code user lobbyStore  = liftIO $ atomically $ do
  -- TODO: there has to be a better way to do this with eithers
  lobbies <- readTVar lobbyStore
  let newLobbies = addProfileToLobby' code user lobbies 
  case newLobbies of
    Left a -> pure  (LobbyError a)
    Right newLobbies -> do  writeTVar lobbyStore  newLobbies
                            pure $ maybe (LobbyError NoLobby) (\lobby -> NewLobbyState lobby) (H.lookup code newLobbies)

makeNewGame :: ( WithDb env m
               )
            => Lobby -> RoomState -> StdGen -> m MsgOut
makeNewGame Lobby{..} RoomState{..} gen = do
  newGame <- initGame lobbyCode lobbyMembers gen
  liftIO $ atomically $ do
    modifyTVar' gameStore (\games -> H.insert lobbyCode newGame games)
    modifyTVar' lobbyStore (\lobbies -> H.delete lobbyCode lobbies)
  pure $ NewGameState newGame

lookupLobby :: (MonadIO m,
                MonadReader env m,
                Has RoomState env
               ) =>  RoomCode -> m (Maybe Lobby)
lookupLobby roomCode = do
  roomSt <- grab @RoomState 
  liftIO $ atomically $ do
    lobbies <- readTVar $ lobbyStore roomSt
    pure $ H.lookup roomCode lobbies
  
  
maxCapacity :: Int
maxCapacity = 6

lobbyActionHandler :: ( WithDb env m,
                       MonadReader env m,
                       Has RoomState env 
                      ) =>  LobbyAction -> m MsgOut
lobbyActionHandler action = do
  r@RoomState{..} <- grab @RoomState
  gen <- liftIO $ getStdGen 
  case action of
    NewLobby user -> createLobby user lobbyStore  
    JoinLobby roomCode user -> addProfileToLobby roomCode user lobbyStore
    InitGame lobby -> makeNewGame lobby r gen 


    -- AddQuestion roomCode question -> addQuestionToLobby roomCode question lobbyStore

    

