-----------------------------------------------------------
-- |
-- Module      :  Database.HaskellDB.Connect.HDBC.Simple
-- Copyright   :  Kei Hibino 2012
-- License     :  BSD-style
--
-- Maintainer  :  ex8k.hibino@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Bracketed HaskellDB session with 'IO'.
--
-----------------------------------------------------------
module Database.HaskellDB.Connect.HDBC.Simple (
  hdbcSession
  ) where

import Database.HDBC (IConnection)
import Database.HaskellDB.Database (Database)
import Database.HaskellDB.Sql.Generate (SqlGenerator)
import Database.HaskellDB.Connect.HDBC (makeHDBCSession)

import Control.Exception (bracket)

-- | Run an action on a HDBC 'IConnection' and close the connection.
--   Simple 'IO' version.
hdbcSession :: IConnection conn
            => SqlGenerator
            -> IO conn                    -- ^ Connect action
	    -> (conn -> Database -> IO a) -- ^ Transaction body
            -> IO a
hdbcSession gen =  makeHDBCSession bracket id gen
