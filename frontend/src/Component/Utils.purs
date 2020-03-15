-- | Some utilities are useful across any component. We'll maintain them in this catch-all module.
module Component.Utils where

import Prelude

import Control.Monad.Rec.Class (forever)
import Effect.Aff (Milliseconds(..), delay, error, forkAff, killFiber)
import Effect.Aff.Bus as Bus
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.Query.EventSource as ES

type OpaqueSlot slot = forall query. H.Slot query Void slot

busEventSource :: forall m r act. MonadAff m => Bus.BusR' r act -> ES.EventSource m act
busEventSource bus =
  ES.affEventSource \emitter -> do
    fiber <- forkAff $ forever $ ES.emit emitter =<< Bus.read bus
    pure (ES.Finalizer (killFiber (error "Event source closed") fiber))

-- | subscribe to a ping every 'length' seconds
timerEventSource length = ES.affEventSource \emitter -> do
  fiber <- forkAff $ forever $   ES.emit emitter =<<  (delay (Milliseconds (length * 1000.0)))
  pure (ES.Finalizer (killFiber (error "Event source closed") fiber))
