module Component.Timer where

import Effect.Timer
import Prelude

import Component.Utils (timerEventSource)
import Control.Monad.Rec.Class (forever)
import Data.Const (Const(..))
import Data.Maybe (Maybe(..))
import Data.Time.Duration (Seconds(..))
import Effect.Aff (Milliseconds(..), delay, error, forkAff, killFiber, launchAff_)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Query.EventSource as ES
import UI.UI (css)


type State = { duration :: Int}

data Action = Receive Int | Tick Unit | Initialize


component ::  forall m .
              MonadAff m  =>
              H.Component HH.HTML (Const Void) Int Int m
component = H.mkComponent { initialState : initialState
                          , render
                          , eval : H.mkEval $ H.defaultEval { initialize = Just Initialize
                                                            , receive  = Just <<< Receive
                                                            , handleAction = handleAction
                                                            }
                          }

  where
    initialState = \duration  -> { duration : duration}
    handleAction = case _ of
      Receive time -> H.modify_ _ {duration = time}

      Initialize -> do
        -- void $ liftEffect $ setInterval 100 (H.lift $ void $  handleAction Tick)
        _ <- H.subscribe (Tick <$> timerEventSource 1.0)
        pure unit
      Tick a -> do
        st <- H.get
        H.modify_ _ { duration = max 0 (st.duration - 1)}
        H.get >>= \st -> H.raise st.duration

    render st =
      HH.div [ css "container"]
      [
        HH.div [ css "tag is-centered is-large is-warning" ]
        [ 
          HH.figure [ css "media-left"]
          [
            HH.span [ css "icon" ] [ HH.i [ css "fas fa-clock" ] [] ]
          ]
        , HH.p [ css "is-size-3 "]
          [
            HH.text $ show st.duration
          ]
        ]
      ]
