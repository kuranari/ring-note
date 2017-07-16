module Lib
    ( examination
    ) where

import Control.Monad (forM_)
import Data.Maybe
import System.IO (hFlush, stdout)
import System.Console.ANSI
import System.Console.Readline (readline)
import Data.Algorithm.Diff (getGroupedDiff, Diff(First, Second, Both))

sentences :: [(String, String)]
sentences = [("これはリンゴです。", "This is an apple.")
            ,("彼は東京に住んでいます。", "He lives in Tokyo.")
            ,("これらの本はとても高い", "These books are very expensive.")
            ]

examination :: IO ()
examination = forM_ sentences $ \(question, answer) -> do
  putStrLn question
  hFlush stdout

  maybeLine <- readline "> "
  case maybeLine of
    Nothing -> return ()
    Just line -> putResult answer line

putResult :: String -> String -> IO ()
putResult expected actual = do
  if expected == actual then
    putStrLn "✅"
  else do
    putStrLn "====="
    putStrLn expected
    mapM_ showDiff $ getGroupedDiff actual expected
    putStrLn ""
  putStrLn ""

showDiff :: Diff String -> IO ()
showDiff (First x) = do
  setSGR [SetUnderlining SingleUnderline]
  setSGR [SetColor Foreground Dull Red]
  putStr x
  setSGR [Reset]
showDiff (Second x) = do
  setSGR [SetUnderlining SingleUnderline]
  setSGR [SetColor Foreground Dull Green]
  putStr x
  setSGR [Reset]
showDiff (Both x _) = do
  putStr x
