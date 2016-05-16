module  DatabaseConn where
--import	Yesod
import	Database.HDBC.Sqlite3 (connectSqlite3)
import	Database.HDBC (run, quickQuery, fromSql, toSql)
import  Database.HDBC.SqlValue
import  Control.Monad
import	Data.Tree

--getUsers :: Int -> IO()

runDB = connectSqlite3 "haskell.db"
          
 
getUsers = do 
  	     conn <- connectSqlite3 "haskell.db"
             run conn "INSERT INTO history (user_id) VALUES (1)" []
	     run conn "INSERT INTO history (user_id) VALUES (2)" []

 	     r <- quickQuery conn
               "SELECT user_id, operation from history ORDER BY user_id"
               []

             let results = map convRow r
             return $ results


convRow [sqlId, sqlPassword] = do
                pass
                where pass = (fromSql sqlId)::String
convRow x = fail $ "Error: " ++ show x
                      

