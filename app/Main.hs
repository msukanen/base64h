module Main where

--
-- Yolo
--
import Base64 (encode, decode)

x64 :: String
x64 = "This is a brown fox. What does the fox say? Bip-blibi-bibi-blibibi-bli!"

-- / 'main'
main :: IO()
main = do
    let b64 = encode x64
    print x64
    print b64
    print $ decode b64
