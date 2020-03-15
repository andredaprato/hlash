module AppM where

import Control.Monad.Reader.Trans
import Effect.Aff
import Env
import Prelude

import API.Endpoint (Endpoint(..))
import API.Request (RequestMethod(..))
import API.Request as Request
import API.Utils (authenticate, decode, mkRequest)
import Capability.Navigate (class Navigate, navigate, logout)
import Capability.Resource.Game (class ManageGame)
import Capability.Resource.Lobby (class ManageLobby, initGame)
import Capability.Resource.Question (class ManageQuestion)
import Capability.Resource.User (class ManageUser)
import Data.Argonaut (encodeJson, inferEmpty)
import Data.Maybe (Maybe(..))
import Data.Route as Route
import Effect.Aff.Bus as Bus
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Ref as Ref
import Routing.Duplex (print)
import Routing.Hash (setHash)
import Socket.Game (submitAnswer, voteForQuestion)
import Socket.Lobby as Socket
import Type.Equality (class TypeEquals, from)


newtype AppM a = AppM (ReaderT Env Aff a)

runAppM :: Env -> AppM ~> Aff
runAppM env (AppM m) = runReaderT m env

derive newtype instance functorAppM :: Functor AppM
derive newtype instance applyAppM :: Apply AppM
derive newtype instance applicativeAppM :: Applicative AppM
derive newtype instance bindAppM :: Bind AppM
derive newtype instance monadAppM :: Monad AppM
derive newtype instance monadEffectAppM :: MonadEffect AppM
derive newtype instance monadAffAppM :: MonadAff AppM 

instance monadAskAppM :: TypeEquals e Env => MonadAsk e AppM where
  ask = AppM $ asks from

instance navigateAppM :: Navigate AppM where
  navigate =
    liftEffect <<< setHash <<< print Route.router

  logout = do
    { currentUser, userBus } <- asks _.userEnv
    liftEffect do
      Ref.write Nothing currentUser
      -- Request.removeToken
    liftAff do
      Bus.write Nothing userBus
    navigate Route.Login
instance manageUserAppM :: ManageUser AppM where
  loginUser = authenticate Request.login
  registerUser = authenticate Request.register
  logoutUser =
    let method = Get in
    mkRequest {endpoint: Logout, method} >>= const (pure unit)
        

instance manageQuestionAppM :: ManageQuestion AppM where
  makeQuestion body = 
    let method = Post $ Just $ encodeJson body in
    void $ mkRequest {endpoint : MakeQuestion, method} 
  getQuestions =
    let method = Get in
    mkRequest {endpoint: GetQuestion, method} >>= decode

instance manageLobbyAppM :: ManageLobby AppM where
  makeLobby = Socket.newLobby
  joinLobby = Socket.joinLobby
  initGame = Socket.initGame 

instance manageGameAppM :: ManageGame AppM where
  submitAnswer = submitAnswer
  voteForQuestion = voteForQuestion
