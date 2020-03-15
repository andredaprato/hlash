{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
module Hlash.Server.Socket.Message where

import           Control.Concurrent
import           Control.Concurrent.Async.Lifted
import           Control.Monad.Loops
import           Control.Monad.Trans.Control
import           Data.Aeson
import qualified Data.HashMap.Strict as M
import           Network.WebSockets
import           System.Random

import           Hlash.Core.Types
import           Hlash.Core.Game.Game
import           Hlash.Core.Profile
import           Hlash.Core.Lobby
import           Hlash.App.Error

import           Hlash.Db
import           Hlash.Effects.RoomCode
import           Hlash.App.Env



msgLoopWithTimeout :: ( MonadIO m,
                        MonadBaseControl IO m,
                        MonadReader env m,
                        Has RoomState env,
                        WithDb env m
                      ) => 
                      Client -> m ()
msgLoopWithTimeout c@Client{..} = msgLoop c >>= go

  where go (state, roomCode) = case state of
          GameState GameOver _ -> msgLoopWithTimeout c
          GameState _ _ -> do
            loop    <- async $ msgLoop c
            timeout <- async $ checkGameStageTimeout c state roomCode

            waitEitherCancel loop timeout >>= \case
              Left newState ->  go  newState
              Right (Just newState) -> go newState
              Right Nothing -> (liftIO $ print " game not found ") >> msgLoopWithTimeout c


  
msgLoop :: ( MonadIO m,
             MonadReader env m,
             Has RoomState env,
             WithDb env m
           ) =>
           Client -> m (GameState, RoomCode)
msgLoop c@Client{..} = untilJust $ do
    msg <- liftIO  $  receiveData ws
    case  eitherDecode  msg of
      Left err  -> liftIO $ print ("decode gameaction error: " <> err) >> pure Nothing
      Right msg -> runMsgHandler c msg

checkGameStageTimeout :: ( MonadIO m
                         , MonadReader env m
                         , Has RoomState env
                         ) =>
                         Client -> GameState ->  RoomCode -> m (Maybe (GameState, RoomCode))
checkGameStageTimeout c prevGameState roomCode =  do
  liftIO $ case prevGameState of
    GameState _ AwaitingQuestion -> threadDelay (awaitingQuestionTimeout * second)
    GameState _ (Display _) -> threadDelay (displayIndexTimeout * second)

  r@RoomState{..} <- grab @RoomState
  broadcast <- liftIO $ atomically $ do
    games <- readTVar gameStore
    case M.lookup roomCode games of
      Nothing -> pure $ (sendMsg c $ GameError DoesNotExist) >> pure  Nothing
      Just g@Game{..} -> if gameState == prevGameState

                         then  pure $ do (msg, mNewGameStage) <- runGameAction r roomCode pAction
                                         broadcastGameAction g c msg
                                         case mNewGameStage of
                                           Nothing -> pure Nothing
                                           Just newGameStage -> pure $ Just (newGameStage, gameCode)

                         else pure . pure $ Just (gameState, gameCode)

  -- run the action we computed in stm
  broadcast
  

    where  pAction = PlayerAction { action = ForceNextStage
                                  , player = Profile "admin"}  -- hacky, maybe write a non-player action function
  
           fst3 (a,b,c) = a
           second = 1000000
     
-- | Receives all decoded websocket messages,
-- | and returns a new game state if the action produces one, otherwise it returns
-- | nothing.
runMsgHandler :: ( WithDb env m,
                   MonadReader env m,
                   Has RoomState env ) =>
                 Client -> MsgIn -> m (Maybe (GameState, RoomCode))
runMsgHandler c@Client{..} msgIn = do
  roomSt <- grab @RoomState 
  case msgIn of 
    LobbyMsg lobbyAction -> do
      msgOut <- lobbyActionHandler lobbyAction
      case msgOut of
        NewLobbyState lobby     -> broadcastLobbyAction lobby c msgOut  >> pure Nothing
        NewGameState g@Game{..} -> do broadcastGameAction g c msgOut
                                      pure  (Just (gameState , gameCode ))
        _                       -> sendMsg c msgOut >> pure Nothing

    GameMsg (GameMsgIn gameAction roomCode) -> do
        (msgOut, mGameState) <- runGameAction roomSt roomCode pAction 
        case msgOut of
          NewGameState game -> broadcastGameAction game c msgOut
          _                 -> sendMsg c msgOut

        pure $ (,roomCode) <$> mGameState 
            where pAction = PlayerAction { player = profile, action = gameAction }

    RequestRoom roomCode -> do
      mLobby <- lookupLobby roomCode 
      mGame <- lookupGame roomCode 
      case mLobby of
        Nothing -> case mGame of
          Nothing -> sendMsg c (LobbyError NoLobby) >> pure Nothing
          Just game -> sendMsg c (NewGameState game) >> pure Nothing
        Just lobby -> sendMsg c (NewLobbyState lobby) >> pure Nothing


  
                             


sendMsg :: MonadIO m => Client -> MsgOut -> m ()
sendMsg Client{..} = liftIO . sendTextData ws . encodeWithTags

broadcast :: MonadIO m => [Client] -> MsgOut -> m ()
broadcast clients msg = liftIO $ forM_ clients (\c -> sendMsg c msg)

broadcastLobbyAction  lobby client msg =  do
  filterClients (lobbyMembers lobby) >>= \clients -> broadcast clients  msg  

broadcastGameAction :: (MonadIO m,
                        MonadReader env m,
                        Has RoomState env) => Game -> Client -> MsgOut -> m ()
broadcastGameAction g client msg =  
  filterClients (members g) >>= \clients -> broadcast clients  msg

                                     
filterClients :: ( MonadIO m,
                   MonadReader env m,
                   Has RoomState env) => [Profile] -> m [Client]
filterClients members = do roomSt <- grab @RoomState
                           liftIO $ atomically $ do
                             M.elems <$> readTVar (clients roomSt)
                               >>= (pure . filter (\client -> (profile client) `elem` members )
                                 )


                               
lookupGame :: (MonadIO m,
                MonadReader env m,
                Has RoomState env
               ) =>  RoomCode -> m (Maybe Game)
lookupGame roomCode = do
  roomSt <- grab @RoomState 
  liftIO $ atomically $ do
    games <- readTVar $ gameStore roomSt
    pure $ M.lookup roomCode games

-- |  An output message and the new game state if the state has changed
runGameAction :: MonadIO m
                  => RoomState -> RoomCode -> PlayerAction -> m (MsgOut, Maybe GameState)
runGameAction RoomState{..} roomCode  pAction = liftIO getStdGen >>= \gen -> liftIO $  atomically $ do
          allGames <- readTVar gameStore
          let mGame = M.lookup roomCode allGames
          case mGame of
            Nothing ->  pure $ (GameError DoesNotExist, Nothing)
            Just game ->
              do case runAction gen game pAction of
                   Left err -> pure $ (GameError err,  Nothing)
                   Right newGame -> do
                     -- write in modified gamestate to TVar
                     writeTVar gameStore  (M.insert roomCode newGame allGames)

                     let gameStateChanged = gameState newGame /= gameState game

                     pure $ (NewGameState newGame, if gameStateChanged
                                                      then Just $ gameState newGame
                                                      else Nothing)
