{-# LANGUAGE FlexibleContexts #-}
module Prelude (
  module Relude,
  module Lens,
  module Json,
  module Web,
  module Sql, 
  encodeWithTags,
  decodeWithTags
               )
  where

import Relude
import Control.Lens as Lens ((^.), (.~))
import Data.Aeson as Json (FromJSON (parseJSON), ToJSON (toJSON), )
import Data.Aeson (GToJSON, Zero, SumEncoding(..), Options(..), encode, genericToJSON , defaultOptions)
import GHC.Generics (Rep)
import Database.PostgreSQL.Simple.FromField as Sql (FromField (fromField))
import Database.PostgreSQL.Simple.ToRow as Sql (ToRow (toRow))
import Database.PostgreSQL.Simple.FromRow as Sql (FromRow (fromRow), field)
import Database.PostgreSQL.Simple.ToField as Sql (ToField (toField))
import Servant.API.Generic as Web ((:-), toServant)
import Servant.API as Web ((:>), Capture, Get, Header, Header', JSON, NoContent (NoContent), Post,
                           QueryParam, QueryParam', ReqBody)

import Data.ByteString.Lazy as B

encodeWithTags :: (Generic a, GToJSON Zero (Rep a)) => a -> B.ByteString
encodeWithTags = encode . genericToJSON (defaultOptions {tagSingleConstructors = True, sumEncoding = taggedObject})


decodeWithTags :: (Generic a, GToJSON Zero (Rep a)) => a -> B.ByteString
decodeWithTags = encode . genericToJSON (defaultOptions {tagSingleConstructors = True, sumEncoding = taggedObject})

taggedObject = TaggedObject
  { tagFieldName      = "tag"
  , contentsFieldName = "contents"
  }
