{-# LANGUAGE Rank2Types #-}
-----------------------------------------------------------
-- |
-- Module      :  Database.HaskellDB.Connect.HDBC
-- Copyright   :  Kei Hibino <ex8k.hibino@gmail.com>
-- License     :  BSD-style
--
-- Maintainer  :  ex8k.hibino@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Connection management for HaskellDB with HDBC
--
-----------------------------------------------------------

module Database.HaskellDB.Connect.HDBC (
  makeHDBCSession
  ) where

import Database.HDBC (IConnection, handleSqlError)
import qualified Database.HDBC as HDBC

import Database.HaskellDB.Database (Database (..))
import Database.HaskellDB.Sql.Generate (SqlGenerator)

import Database.HaskellDB.Connect.HDBC.Internal (mkDatabase)


-- | Run an action on a HDBC IConnection and close the connection.
makeHDBCSession :: (Monad m, IConnection conn)
                => (m conn -> (conn -> m ()) -> (conn -> m a) -> m a) -- ^ bracket
                -> (forall b. IO b -> m b)                            -- ^ lift
                -> SqlGenerator
                -> IO conn                                            -- ^ Connect action
                -> (conn -> Database -> m a)                          -- ^ Transaction body
                -> m a
makeHDBCSession bracket lift gen connect action =
  bracket
    (lift $ handleSqlError connect)
    (lift
     . handleSqlError
     . HDBC.disconnect)
    (\conn -> do
        x <- action conn (mkDatabase gen conn)
        -- Do rollback independent from driver default behavior when disconnect.
        lift $ HDBC.rollback conn
        return x)
