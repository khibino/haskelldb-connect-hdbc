{-# LANGUAGE FlexibleContexts #-}
-----------------------------------------------------------
-- |
-- Module      :  Database.HaskellDB.Connect.HDBC.Lifted
-- Copyright   :  Kei Hibino
-- License     :  BSD-style
--
-- Maintainer  :  ex8k.hibino@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-----------------------------------------------------------
module Database.HaskellDB.Connect.HDBC.Lifted (
  hdbcSession
  ) where

import Database.HDBC (IConnection)
import Database.HaskellDB.Database (Database)
import Database.HaskellDB.Sql.Generate (SqlGenerator)
import Database.HaskellDB.Connect.HDBC (makeHDBCSession)

import Control.Monad.Base (liftBase)
import Control.Monad.Trans.Control (MonadBaseControl)
import Control.Exception.Lifted (bracket)


-- | Run an action on a HDBC IConnection and close the connection.
--   'MonadBaseControl' 'IO' version.
hdbcSession :: (MonadBaseControl IO m, IConnection conn)
            => SqlGenerator
            -> IO conn                   -- ^ Connect action
	    -> (conn -> Database -> m a) -- ^ Transaction body
            -> m a
hdbcSession gen =  makeHDBCSession bracket liftBase gen
