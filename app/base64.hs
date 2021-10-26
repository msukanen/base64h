module Base64 (encode, decode) where

import Data.Bits ( Bits((.&.), shiftL, shiftR) )
import Data.Char (ord, chr)
import Data.List (elemIndex)
import Data.Maybe ( fromJust )
import GHC.Word (leWord32)

chops :: Int -> [e] -> [[e]]
chops i ls = map (take i) (build (split ls)) where
  split :: [e] -> ([e] -> a -> a) -> a -> a
  split [] _ n = n
  split l c n  = l `c` split (drop i l) c n
  build :: ((a -> [a] -> [a]) -> [a] -> [a]) -> [a]
  build g = g (:) []

base64_set :: [Char]
base64_set = ['A','B','C','D','E','F','G','H','I','J','K','L','M',
              'N','O','P','Q','R','S','T','U','V','W','X','Y','Z',
              'a','b','c','d','e','f','g','h','i','j','k','l','m',
              'n','o','p','q','r','s','t','u','v','w','x','y','z',
              '0','1','2','3','4','5','6','7','8','9','+','/']

encode :: String -> String
encode s = padx (length s `rem` 3) s where
  padx :: Int -> [Char] -> [Char]
  padx 0 s = chunkt s
  padx 1 s = init (init (chunkt (s ++ ['\0', '\0']))) ++ ['=','=']
  padx 2 s = init (chunkt (s ++ ['\0'])) ++ ['=']
  chunkt s = dicet $ chops 3 s where
    dicet :: [[Char]] -> [Char]
    dicet [x] = encode' x
    dicet (x:xs) = encode' x ++ dicet xs
    encode' :: [Char] -> [Char]
    encode' [a,b,c] = z $ y (ord a) (ord b) (ord c) where
      y :: Int -> Int -> Int -> Int
      y a' b' c' = ((0xff .&. a') `shiftL` 16) + ((0xff .&. b') `shiftL` 8) + (0xff .&. c')
      z :: Int -> [Char]
      z x = [base64_set !! ((x `shiftR` 18) .&. 0x3f),
             base64_set !! ((x `shiftR` 12) .&. 0x3f),
             base64_set !! ((x `shiftR`  6) .&. 0x3f),
             base64_set !! ( x              .&. 0x3f)]

decode :: String -> String
decode s = unpadx (chops 4 $ clensed s) where
  clensed s = s
  unpadx [x] = decode' x
  unpadx (x:xs) = decode' x ++ unpadx xs
  decode' [a,b,'=','='] = init $ init $ decode' [a,b,'A','A']
  decode' [a,b,c,'='] = init $ decode' [a,b,c,'A']
  decode' [a,b,c,d] = z $ y a `shiftL` 18 + y b `shiftL` 12 + y c `shiftL` 6 + y d where
    y :: Char -> Int
    y x = fromJust $ elemIndex x base64_set
    z :: Int -> [Char]
    z v = [chr $ 0xff .&. (v `shiftR` 16), chr $ 0xff .&. (v `shiftR` 8), chr $ 0xff .&. v]
