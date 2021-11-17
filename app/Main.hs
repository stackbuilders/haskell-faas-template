module Main (main) where

import Server (server)
-- Event/Context
main :: IO ()
main = server 5000

