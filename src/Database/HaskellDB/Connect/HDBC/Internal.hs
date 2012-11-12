{-# LANGUAGE CPP #-}
-----------------------------------------------------------
-- |
-- Module      :  Database.HaskellDB.Connect.HDBC.Internal
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

module Database.HaskellDB.Connect.HDBC.Internal (
  mkDatabase
  ) where

import Data.Char (toLower)
import Data.Maybe (fromMaybe)
import Data.Map (Map)
import qualified Data.Map as Map

import Database.HDBC
  (IConnection, SqlColDesc (..), SqlTypeId (..),
   handleSqlError, getColumnNames, fetchAllRows)
import qualified Database.HDBC as HDBC

import Database.HaskellDB (Rel, Record)
import Database.HaskellDB.Database
  (Database (..), GetRec (getRec), GetInstances (..))
import Database.HaskellDB.Sql.Generate
  (SqlGenerator, sqlQuery, sqlInsert, sqlInsertQuery, sqlDelete, sqlUpdate,
   sqlCreateDB, sqlCreateTable, sqlDropDB, sqlDropTable)
import Database.HaskellDB.Sql.Print
  (ppSql, ppInsert, ppDelete, ppUpdate, ppCreate, ppDrop)
import Database.HaskellDB.PrimQuery
  (PrimQuery (..), PrimExpr, Scheme, attributes, TableName, Assoc, Attribute)
import Database.HaskellDB.FieldType (FieldDesc, FieldType (..))

mkDatabase :: (IConnection conn) => SqlGenerator -> conn -> Database
mkDatabase gen connection
    = Database { dbQuery	= hdbcQuery       gen connection,
    		 dbInsert	= hdbcInsert      gen connection,
		 dbInsertQuery 	= hdbcInsertQuery gen connection,
		 dbDelete	= hdbcDelete      gen connection,
		 dbUpdate	= hdbcUpdate      gen connection,
		 dbTables       = hdbcTables          connection,
		 dbDescribe     = hdbcDescribe        connection,
		 dbTransaction  = hdbcTransaction     connection,
#if MIN_VERSION_haskelldb(2,1,1)
                 dbCommit       = HDBC.commit         connection,
#endif
		 dbCreateDB     = hdbcCreateDB    gen connection,
		 dbCreateTable  = hdbcCreateTable gen connection,
		 dbDropDB       = hdbcDropDB      gen connection,
		 dbDropTable    = hdbcDropTable   gen connection
	       }

hdbcQuery :: (GetRec er vr, IConnection conn) =>
	     SqlGenerator
          -> conn
	  -> PrimQuery
	  -> Rel er
	  -> IO [Record vr]
hdbcQuery gen connection q rel = hdbcPrimQuery connection sql scheme rel
    where sql = show $ ppSql $ sqlQuery gen q
          scheme = attributes q

hdbcInsert :: (IConnection conn) => SqlGenerator -> conn -> TableName -> Assoc -> IO ()
hdbcInsert gen conn table assoc =
    hdbcPrimExecute conn $ show $ ppInsert $ sqlInsert gen table assoc

hdbcInsertQuery :: (IConnection conn) => SqlGenerator -> conn -> TableName -> PrimQuery -> IO ()
hdbcInsertQuery gen conn table assoc =
    hdbcPrimExecute conn $ show $ ppInsert $ sqlInsertQuery gen table assoc

hdbcDelete :: (IConnection conn) => SqlGenerator -> conn -> TableName -> [PrimExpr] -> IO ()
hdbcDelete gen conn table exprs =
    hdbcPrimExecute conn $ show $ ppDelete $ sqlDelete gen table exprs

hdbcUpdate :: (IConnection conn) => SqlGenerator -> conn -> TableName -> [PrimExpr] -> Assoc -> IO ()
hdbcUpdate gen conn table criteria assigns =
    hdbcPrimExecute conn $ show $ ppUpdate $ sqlUpdate gen table criteria assigns

hdbcTables :: (IConnection conn) => conn -> IO [TableName]
hdbcTables conn = handleSqlError $ HDBC.getTables conn

hdbcDescribe :: (IConnection conn) => conn -> TableName -> IO [(Attribute,FieldDesc)]
hdbcDescribe conn table =
    handleSqlError $ do
                     cs <- HDBC.describeTable conn table
                     return [(n,colDescToFieldDesc c) | (n,c) <- cs]

colDescToFieldDesc :: SqlColDesc -> FieldDesc
colDescToFieldDesc c = (t, nullable)
    where
    nullable = fromMaybe True (colNullable c)
    string = maybe StringT BStrT (colSize c)
    t = case colType c of
            SqlCharT          -> string
            SqlVarCharT       -> string
            SqlLongVarCharT   -> string
            SqlWCharT	      -> string
            SqlWVarCharT      -> string
            SqlWLongVarCharT  -> string
            SqlDecimalT       -> IntegerT
            SqlNumericT       -> IntegerT
            SqlSmallIntT      -> IntT
            SqlIntegerT	      -> IntT
            SqlRealT	      -> DoubleT
            SqlFloatT	      -> DoubleT
            SqlDoubleT	      -> DoubleT
            SqlBitT	      -> BoolT
            SqlTinyIntT	      -> IntT
            SqlBigIntT	      -> IntT
            SqlBinaryT	      -> string
            SqlVarBinaryT     -> string
            SqlLongVarBinaryT -> string
            SqlDateT          -> CalendarTimeT
            SqlTimeT          -> CalendarTimeT
#if MIN_VERSION_haskelldb(2,2,1)
            SqlTimestampT     -> LocalTimeT
#endif
            SqlUTCDateTimeT   -> CalendarTimeT
            SqlUTCTimeT       -> CalendarTimeT
            SqlTimeWithZoneT  -> CalendarTimeT
            SqlTimestampWithZoneT -> CalendarTimeT
            SqlIntervalT _    -> string
            SqlGUIDT          -> string
            SqlUnknownT _     -> string

hdbcCreateDB :: (IConnection conn) => SqlGenerator -> conn -> String -> IO ()
hdbcCreateDB gen conn name
    = hdbcPrimExecute conn $ show $ ppCreate $ sqlCreateDB gen name

hdbcCreateTable :: (IConnection conn) => SqlGenerator -> conn -> TableName -> [(Attribute,FieldDesc)] -> IO ()
hdbcCreateTable gen conn name attrs
    = hdbcPrimExecute conn $ show $ ppCreate $ sqlCreateTable gen name attrs

hdbcDropDB :: (IConnection conn) => SqlGenerator -> conn -> String -> IO ()
hdbcDropDB gen conn name
    = hdbcPrimExecute conn $ show $ ppDrop $ sqlDropDB gen name

hdbcDropTable :: (IConnection conn) => SqlGenerator -> conn -> TableName -> IO ()
hdbcDropTable gen conn name
    = hdbcPrimExecute conn $ show $ ppDrop $ sqlDropTable gen name

-- | HDBC implementation of 'Database.dbTransaction'.
hdbcTransaction :: (IConnection conn) => conn -> IO a -> IO a
hdbcTransaction conn action =
    handleSqlError $ HDBC.withTransaction conn (\_ -> action)


-----------------------------------------------------------
-- Primitive operations
-----------------------------------------------------------

type HDBCRow = Map String HDBC.SqlValue

normalizeField :: String -> String
normalizeField =  map toLower

-- | Primitive query
hdbcPrimQuery :: (GetRec er vr, IConnection conn) =>
		 conn -- ^ Database connection.
	      -> String     -- ^ SQL query
	      -> Scheme     -- ^ List of field names to retrieve
	      -> Rel er   -- ^ Phantom argument to get the return type right.
	      -> IO [Record vr]    -- ^ Query results
hdbcPrimQuery conn sql scheme rel =
    do
    stmt <- handleSqlError $ HDBC.prepare conn sql
    _    <- handleSqlError $ HDBC.execute stmt []
    rows <- fetchNormalizedAllRowsAL stmt
    mapM (getRec hdbcGetInstances rel scheme) $ map Map.fromList rows
  where fetchNormalizedAllRowsAL sth =
          do
          names <- map normalizeField `fmap` getColumnNames sth
          rows <- fetchAllRows sth
          return $ map (zip names) rows

-- | Primitive execute
hdbcPrimExecute :: (IConnection conn) => conn -- ^ Database connection.
		-> String     -- ^ SQL query.
		-> IO ()
hdbcPrimExecute conn sql =
    do
    _ <- handleSqlError $ HDBC.run conn sql []
    return ()


-----------------------------------------------------------
-- Getting data from a statement
-----------------------------------------------------------

hdbcGetInstances :: GetInstances HDBCRow
hdbcGetInstances =
    GetInstances {
		  getString        = hdbcGetValue
		 , getInt          = hdbcGetValue
		 , getInteger      = hdbcGetValue
		 , getDouble       = hdbcGetValue
		 , getBool         = hdbcGetValue
		 , getCalendarTime = hdbcGetValue
#if MIN_VERSION_haskelldb(2,2,1)
		 , getLocalTime    = hdbcGetValue
#endif
		 }

-- hdbcGetValue :: Data.Convertible.Base.Convertible SqlValue a
--             => HDBCRow -> String -> IO (Maybe a)
hdbcGetValue m f = case Map.lookup (normalizeField f) m of
                     Nothing -> fail $ "No such field " ++ f
                     Just x  -> return $ HDBC.fromSql x
