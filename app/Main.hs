module Main where

--
-- Yolo
-- Molo
--
import System.Environment ( getArgs )

-- / 'main'
main :: IO()
main = getArgs >>= print . haqify . head

haqify s = "Haq! " ++ s
