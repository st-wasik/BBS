module Main where

import Data.Bits as Bit
import Data.Matrix 
import Data.List
import System.Random
import Numeric.Decimal

main :: IO ()
main = do
    putStr "Bit length: "
    bitLength <- getLine
    g1 <- newStdGen
    g2 <- newStdGen
    let b = blum g1 g2
    let a = getRandomSeed b

    -- putStrLn . show . fromList 20 20 . fmap (\x-> if x==0 then ' ' else 'X') $ bbs 400 b a
    let bbs_ =  bbs (read bitLength :: Int) b a
    putStrLn ("seed  = " ++ show a)
    putStrLn ("blum  = " ++ show b)
    putStrLn ((++) "rand  = " . show $ bitsToInt bbs_)
    putStrLn ((++) "bits  = " . bitsToString $ show bbs_)

    let (odds,evens) = partition (==1) bbs_
    putStrLn "\nCount"
    putStrLn ((++) "1s    = " . show $ length odds)
    putStrLn ((++) "0s    = " . show $ length evens)
    putStrLn ((++) "all   = " . show $ length evens + (length odds))

    putStrLn "\nTests"
    putStrLn ((++) "freqT = " . show $ freqTest bbs_)
    let twoBit = twoBitTest bbs_
    putStrLn $ ((++) "2bitT = " . show $ twoBit) ++ "  :: [00,01,10,11]"
    --putStrLn $ ((++) "2bit% = " . show $ twoBitP twoBit) ++ "  :: [%]"


blum :: RandomGen a => a -> a -> Int
blum gen1 gen2 = p*q
    where p = head $ dropWhile (\a -> not $ (a `mod` 4) == 3) rands1
          q = head $ dropWhile (\a -> not $ (a `mod` 4) == 3) rands2
          rands1 = randoms gen1
          rands2 = randoms gen2

getRandomSeed :: Int -> Int
getRandomSeed blumInt = if null a then 2 else head a
    where a = take 1 $ dropWhile (\a -> not $ (gcd a blumInt) == 1) blumList
          blumList = if blumInt > 0 then [blumDiv..(blumInt-1)] else [blumDiv, (blumDiv-1)..(blumInt+1)]
          blumDiv = (quot blumInt 8192*128)

bbs :: Int -> Int -> Int -> [Int]
bbs len blum seed = bbs' 1 len blum [seed]

bbs' :: Int -> Int -> Int -> [Int] -> [Int]
bbs' i r n result
    | i >= r = fmap (.&. 1) result
    | otherwise = bbs' (i+1) r n s 
    where xi = (xi_1 ^ 2) `mod` n
          s = result ++ [xi]
          xi_1 = result !! (i-1)

bitsToInt :: [Int] -> Int
bitsToInt bits = foldl (\acc (num, val) -> if val /= 0 then acc + bit num else acc) 0 zipped
    where zipped = zip [0..] bits

freqTest :: [Int] -> Int
freqTest list = foldl (+) 0 mpd 
    where mpd = map (\x-> if x == 0 then -1 else 1) list

twoBitTest list = twoBitTest' list (0,0,0,0)

twoBitTest' (0:0:bits) (oo,ol,lo,ll) = twoBitTest' (0:bits) (oo+1, ol,lo,ll)
twoBitTest' (0:1:bits) (oo,ol,lo,ll) = twoBitTest' (1:bits) (oo, ol+1,lo,ll)
twoBitTest' (1:0:bits) (oo,ol,lo,ll) = twoBitTest' (0:bits) (oo, ol,lo+1,ll)
twoBitTest' (1:1:bits) (oo,ol,lo,ll) = twoBitTest' (1:bits) (oo, ol,lo,ll+1)
twoBitTest' [_] x = x
twoBitTest' [] x = x

twoBitP (oo, ol, lo, ll) = (div oo sum, div ol sum, div lo sum, div ll sum)
    where sum = let s = (oo+ol+lo+ll) in if s < 100 then s else div s 100

bitsToString = filter (\x-> x=='1' || x=='0')