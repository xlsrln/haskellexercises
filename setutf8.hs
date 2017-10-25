import System.IO
module Main

main = do hSetEncoding stdin utf8
          hGetEncoding stdin