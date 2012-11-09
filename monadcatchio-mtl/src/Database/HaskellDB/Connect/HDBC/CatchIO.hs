-----------------------------------------------------------
-- |
-- Module      :  Database.HaskellDB.Connect.HDBC.CatchIO
-- Copyright   :  Kei Hibino
-- License     :  BSD-style
--
-- Maintainer  :  ex8k.hibino@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-----------------------------------------------------------
module Database.HaskellDB.Connect.HDBC.CatchIO (
  hdbcSession
  ) where

import Database.HDBC (IConnection)
import Database.HaskellDB.Database (Database)
import Database.HaskellDB.Sql.Generate (SqlGenerator)
import Database.HaskellDB.Connect.HDBC (makeHDBCSession)

import Control.Monad.Trans (liftIO)
import Control.Monad.CatchIO (MonadCatchIO, bracket)

-- | Run an action on a HDBC IConnection and close the connection.
--   'MonadCatchIO' version.
hdbcSession :: (MonadCatchIO m, IConnection conn)
            => SqlGenerator
            -> IO conn                   -- ^ Connect action
	    -> (conn -> Database -> m a) -- ^ Transaction body
            -> m a
hdbcSession gen =  makeHDBCSession bracket liftIO gen
