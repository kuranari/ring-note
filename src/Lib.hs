module Lib
    ( examination
    ) where

import Control.Monad (forM_)
import Data.Maybe
import System.IO (hFlush, stdout)
import System.Console.ANSI
import System.Console.Readline (readline)
import Data.Algorithm.Diff (getGroupedDiff, Diff(First, Second, Both))
import Data.Function (on)

sentences :: [(String, String)]
sentences = [("私はペンを持っています。", "I have a pen.")
            ,("私はリンゴを持っています", "I have an apple.")
            ,("私はペンを持っています", "I have a pen.")
            ,("私はパイナップルを持っています", "I have a pineapple.")
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
    mapM_ showDiff $ (getGroupedDiff `on` words) actual expected
    putStrLn ""
  putStrLn ""

showDiff :: Diff [String] -> IO ()
showDiff (First x) = do
  setSGR [SetUnderlining SingleUnderline]
  setSGR [SetColor Foreground Dull Red]
  putStr $ unwords x
  setSGR [Reset]
  putStr " "
showDiff (Second x) = do
  setSGR [SetUnderlining SingleUnderline]
  setSGR [SetColor Foreground Dull Green]
  putStr $ unwords x
  setSGR [Reset]
  putStr " "
showDiff (Both x _) = do
  putStr $ unwords x
  putStr " "
