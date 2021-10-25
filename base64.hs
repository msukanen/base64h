--
-- Yolo
-- Molo
--
import System.Environment

-- / 'main'
main :: IO()
main = getArgs >>= print . haqify . head

haqify s = "Haq! " ++ s
