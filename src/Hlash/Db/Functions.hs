{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
module Hlash.Db.Functions
  ( WithDb
  , initialisePool
  , asSingleRow
  , withPool
  , runBeamWithPool
  , selectReturningOne
  , query
  ) where

import Hlash.App.Env (DbPool, Has, grab)
import Hlash.App.Error (AppErrorType, WithError, dbError, throwError, throwOnNothingM)

import qualified Data.Pool as Pool
import qualified Database.PostgreSQL.Simple as Sql
import Database.Beam
import Database.Beam.Postgres


-- | Constraint for monadic actions that wants access to database.
type WithDb env m = (MonadReader env m, Has DbPool env, MonadIO m )

-- | Create 'Pool.Pool' by given credentials.
initialisePool :: ByteString -> IO DbPool
initialisePool credentials = Pool.createPool (Sql.connectPostgreSQL credentials) Sql.close 10 5 10

runBeamWithPool :: WithDb env m => Pg b -> m b
runBeamWithPool query = withPool $ \conn -> runBeamPostgres conn query 

selectReturningOne :: (WithDb env m,
                       FromBackendRow Postgres a)
                   => SqlSelect Postgres a
                   -> m (Maybe a)
selectReturningOne query = runBeamWithPool $ runSelectReturningOne query                                         

-- | Perform action that needs database connection.
withPool :: WithDb env m => (Sql.Connection -> IO b) -> m b
withPool f = do
    pool <- grab @DbPool
    liftIO $ Pool.withResource pool f
{-# INLINE withPool #-}

-- | Performs a query with named parameters and returns a list of rows.
query
    :: (WithDb env m, Sql.FromRow res, Sql.ToRow args)
    => Sql.Query
    -> args
    -> m [res]
query q args = withPool $ \conn -> Sql.query conn q args
{-# INLINE query #-}

-- ----------------------------------------------------------------------------
-- -- Error helpers
-- ----------------------------------------------------------------------------
-- | Helper function working with results from a database when you expect
-- only one row to be returned.
asSingleRow :: (WithError m) => m [a] -> m a
asSingleRow res = withFrozenCallStack $ throwOnNothingM
    singleRowError
    (viaNonEmpty head <$> res)

singleRowError :: AppErrorType
singleRowError = dbError "Expected a single row, but got none"
