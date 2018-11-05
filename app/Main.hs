module Main where

import Data.Bits as Bit
import Data.Matrix 
import Data.List
import System.Random
import Numeric.Decimal
import Data.Char
import Data.Number.Erf

main :: IO ()
main = do
    putStr "Bit length: "
    bitLength <- getLine
    g1 <- newStdGen
    g2 <- newStdGen
    g3 <- newStdGen
    let p = getBlumInt g1
    let q = getBlumInt g2
    
    let b = p*q
    let a = getSeed g3 b

    let bbs_ =  x_bbs (read bitLength :: Integer) b a
    
    putStrLn ((++) "bits  = " . bitsToString $ show bbs_)
    putStrLn ("p     = " ++ show p)
    putStrLn ("q     = " ++ show q)
    putStrLn ("seed  = " ++ show a)
    putStrLn ("blum  = " ++ show b)

    let (odds,evens) = partition (==1) bbs_
    putStrLn "\nCount"
    putStrLn ((++) "1s    = " . show $ length odds)
    putStrLn ((++) "0s    = " . show $ length evens)
    putStrLn ((++) "all   = " . show $ length evens + (length odds))

    putStrLn "\nTests"
    putStrLn ((++) "freqT = " . show $ freqTest bbs_)
    let twoBit = twoBitTest bbs_
    putStrLn $ ((++) "2bitT = " . show $ twoBit) ++ "     :: [00,01,10,11]"
    putStrLn $ ((++) "seria = " . show . seriesTest' $ bbs_)  ++ "     :: [1,2,3,4,5,5<]"
    let pokerList = pokerTest bbs_
    putStrLn $ ((++) "poker = " . show $ pokerList)  ++ "  :: [0-15]"
    putStrLn $ ((++) "pok.X = " . show $ pokerTestVal pokerList)

blum :: RandomGen a => a -> a -> Int
blum gen1 gen2 = p*q
    where p = head $ dropWhile (\a -> not $ mod a 4 == 3) rands1
          q = head $ dropWhile (\a -> not $ mod a 4 == 3) rands2
          rands1 = randoms gen1
          rands2 = randoms gen2

getBlumInt :: RandomGen a => a -> Integer
getBlumInt gen1 = head . dropWhile (\a -> not (mod a 4 == 3) || a < 0) $ randoms gen1

getSeed gen3 blumInt = let rands = randoms gen3 in head $ dropWhile (\a -> (gcd a blumInt /= 1) || a < 0 || a >= blumInt) rands

bbs :: Integer -> Integer -> Integer -> [Integer]
bbs len blum seed = bbs' 1 len blum [seed]

bbs' :: Integer -> Integer -> Integer -> [Integer] -> [Integer]
bbs' i r n result
    | i >= r = fmap (.&. 1) result
    | otherwise = bbs' (i+1) r n s 
    where xi = abs $ (xi_1 ^ 2) `mod` n
          s = result ++ [xi]
          xi_1 = result !! (fromIntegral $ i-1)


x_bbs :: Integer -> Integer -> Integer -> [Integer]
x_bbs len blum seed = x_bbs' 1 len blum seed []

x_bbs' :: Integer -> Integer -> Integer -> Integer-> [Integer] -> [Integer]
x_bbs' i r n xi_1 result
    | i > r = result
    | otherwise = x_bbs' (i+1) r n xi s 
    where xi =  abs $ (xi_1 ^ 2) `mod` n
          s = result ++ [xi .&. 1]

bitsToInt :: [Int] -> Int
bitsToInt bits = foldl (\acc (num, val) -> if val /= 0 then acc + bit num else acc) 0 zipped
    where zipped = zip [0..] bits

freqTest :: [Integer] -> Integer
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

bitsToString' :: [Integer] -> String
bitsToString' list = map (\x -> chr . fromIntegral $ x+48) list

sumBits :: String -> Integer
sumBits = foldr (\x acc -> if x == '1' then acc + 1 else acc) 0 

series list = concat $ map (auxiliary) x
    where x = group list

auxiliary x = let l = init x in (map (\_->'0') l) ++ ['1']

seriesTest :: [Integer] -> Double
seriesTest x = erfc (up / down)
    where v = fromInteger $ sumBits s
          s = series x
          pi = fromInteger $ foldr (\x acc -> if x == 1 then acc + 1 else acc) 0 x
          n = fromInteger . toInteger . length . filter (\x-> (head x) == '1' && length x == 2) . group $ bitsToString' x
          up = abs $ (fromInteger v) - 2*n*pi*(1-pi)
          down = 2.0* (sqrt $ 2.0*n) * pi * (1.0-pi)


seriesTest' x = (x1,x2,x3,x4,x5,x6)
    where x1 = length $ filter (\x-> (head x) == '1' && length x == 1) grps
          x2 = length $ filter (\x-> (head x) == '1' && length x == 2) grps
          x3 = length $ filter (\x-> (head x) == '1' && length x == 3) grps
          x4 = length $ filter (\x-> (head x) == '1' && length x == 4) grps
          x5 = length $ filter (\x-> (head x) == '1' && length x == 5) grps
          x6 = length $ filter (\x-> (head x) == '1' && length x >= 6) grps
          grps = group $ bitsToString' x

pokerTest list = pokerTest' list $ replicate 16 0

pokerTest' :: [Integer] -> [Integer] -> [Integer]
pokerTest' list result
    | length list >= 4 = pokerTest' (drop 4 list) $ incNth index result  
    | otherwise = result
    where index = (binToDec . bitsToString' $ take 4 list )

binToDec :: String -> Integer
binToDec = foldl (\acc x -> if x == '0' then 2*acc else 2*acc + 1) 0 

setNth 0 el (_:xs) = el : xs
setNth _ _ [] = []
setNth n el (x:xs) = x : setNth (n-1) el xs

incNth 0 (x:xs) = (x+1) : xs
incNth _ [] = []
incNth n (x:xs) = x : incNth (n-1) xs

pokerTestVal list = let p = sum $ map (^2) list in 16.0 / 5000 * (fromIntegral p) - 5000

groups _ [] = []
groups a list = take a list : (groups a $ drop a list)