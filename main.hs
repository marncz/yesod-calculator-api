{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
import           Yesod hiding (runDB)
import           Data.Text hiding (zip, map, concat, length, pack, drop, head, split)
import		 Data.List.Split
import		 Data.Char
import		 Data.Tree
import		 Data.Text.Lazy hiding (zip, map, head, drop, concat, length,split)
import 		 Prelude hiding (tail, split)
import		 DatabaseConn

data App = App

mkYesod "App" [parseRoutes|
/ HomeR GET
/calc/#String Calc GET
/adduser/#Int User GET
/history/#String History GET
/pi/#Int CalculatePi GET
|]

instance Yesod App

getHomeR :: Handler Html
getHomeR = defaultLayout $ do
 setTitle "Calculator API - Main Page"
 [whamlet|
<title>Calculator API
<h1>Welcome
<h2>Choose one of available API calls below

<li><a href=/calc/2+2>GET /calc/$operation</a>
<p>Calculates given operation and returns result in json

<li><a href=/calc/123/id/1>GET /calc/$operation/id/$user_id</a>
<p>Saves calculation for user with a particular ID

<li><a href=/history/123>GET /history/$user_id</a>
<p> Returns a history of API calls for certain user

<li><a href=/pi/10> GET /pi/$n</a>
<p>Returns calculated PI number up to the Nth place
|]

getCalc :: String -> Handler Html
getCalc calc = do
        let list = split (oneOf "+-/*") calc
	let result = show $ compute list 0
	defaultLayout $ do 
         setTitle "Calculator API - Result Page"
         [whamlet|
<p>Status: OK
<p>Operation: #{calc}
<p>
$forall x <- list
   <li>#{x}
<p>
Result: #{result}
<p>
go back to <a href=/> Main Page </a>
|]


compute :: [[Char]] -> Int -> Int
compute list sum
	   | length list < 2 = sum
	   | isSymbol $ head list !! 0  = do
		 let symb = head list 
 		 let a = read $ list !! 1
		 let new_sum = if(symb == "+") then sum + a else if (symb == "-") then sum - a else if(symb == "*") then sum * a else 0
    	         compute (drop 2 list) new_sum
	   | otherwise = do
		 let a = read $ head list
		 compute (drop 1 list) a 


getUser id = do
	       let userList = getUsers
	       defaultLayout [whamlet| 
 <h1> Added user with id #{id}
 |]
 
getHistory :: String -> Handler Html
getHistory id =  defaultLayout [whamlet|
 <h1>User's History|]

getCalculatePi :: Int -> Handler Html
getCalculatePi n = do
 let pi_n = show $ pi 
 defaultLayout [whamlet|
 <title>Calculator API
 <h1>PI number up to #{n}th place
 <h2>#{pi_n}|]

main :: IO ()
main = do
  getUsers
  warp 3000 App
