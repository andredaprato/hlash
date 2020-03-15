{-# LANGUAGE OverloadedStrings #-}
module Hlash.Config where

import           Toml (TomlCodec, (.=))

import qualified Toml


-- | Data type for the configurable elements of the application.
data Config = Config
    { cDbCredentials :: !ByteString
    }

-- | TOML codec for the 'Config' data type.
configT :: TomlCodec Config
configT = Config
    <$> Toml.byteString "dbCredentials" .= cDbCredentials

-- | Loads the @config.toml@ file.
loadConfig :: MonadIO m => m Config
loadConfig = Toml.decodeFile configT "config.toml"
