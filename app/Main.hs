module Main where

import Event (writeEvents)

main :: IO ()
main = do
  writeEvents 10
