import System
import Array
import Control.Monad.ST
import Data.Array.MArray
import Data.Array.ST
import Text.Printf
import Data.List
import Data.Map hiding (map,(!))
import qualified Data.Map as Map



main :: IO ()
main = getArgs >>= mapM_ histoFile

smoke :: IO ()
smoke =
     do putStr ">> p1cHisto\n\n"
        p1cHisto
        putStr "\n>> histoFile\n\n"
        histoFile "histo.hs"
        putStr "\n>> emptyHisto\n\n"
        emptyHisto
        putStr "\ndone\n"


        
--
-- To generate a histogram of the least significant digits of first 100 primes
--


p1cHisto :: IO ()
p1cHisto = genHisto $ map (`mod` 10) (take 100 primes) 

primes :: [Int]
primes = 2 : sieve [3,5..]
      where
        sieve []    = error "run out of Ints!"
        sieve (p:t) = p : sieve [ n | n<-t, n `mod` p /= 0 ] 



--
-- To find the integers in a file and generate a histogram
--


histoFile :: FilePath -> IO ()
histoFile fnm =
     do putStr $ printf "\n%s\n--------------------\n" fnm
        catch (readFile fnm >>= h_c) oops
      where
        h_c cts = genHisto_ $ [ i | Just i<-map parse (words cts) ]

        oops _  = putStr "<file missing or inaccessible>\n"



--
-- To generate a histogram for empty list
--


emptyHisto :: IO ()
emptyHisto = genHisto []



--
-- Top-level histogram-printing machinery
--


type Histo = Array Int Int


genHisto :: [Int] -> IO ()
genHisto = printHisto False . histo

genHisto_ :: [Int] -> IO ()
genHisto_ = printHisto True . histo

printHisto :: Bool -> Histo -> IO ()
printHisto nz h = mapM_ pr $ indices h
      where
        pr i | nz && n==0 = return ()
             | otherwise  = putStr $ printf "%3d : %s\n" i (replicate n '*')
              where
                n = h!i


--
-- Generating Histograms: wrappers to select functional/monadic/array models
--


histo :: [Int] -> Histo
histo = a_histo



--
-- Generating Histograms: a functional model
--


f_histo :: [Int] -> Histo
f_histo = map2histo . fillHisto . f_histo_

f_histo_ :: [Int] -> Map Int Int
f_histo_ xs = Map.fromList [ (x,1+length t) | x:t <- groupSort xs ]

fillHisto :: Map Int Int -> Map Int Int
fillHisto mp | Map.null mp = mp
             | otherwise   = mp `Map.union` Map.fromList missng
      where
        missng = [ (i,0) | (i,Nothing)<-[(j,Map.lookup j mp)|j<-[mn..mx]] ]
        (mn,_) = Map.findMin mp
        (mx,_) = Map.findMax mp



--
-- Generating Histograms: a monadic model
--


m_histo :: [Int] -> Histo
m_histo = go . (>>> fillHistoH) . histo0_H

histo_H :: [Int] -> HProg ()
histo_H []     = resultis ()
histo_H (i:is) = incH i >>> histo_H is

histo0_H :: [Int] -> HProg ()
histo0_H = mapH_ incH

fillHistoH :: HProg ()
fillHistoH = 
        minH                        >>>= \mn ->
        maxH                        >>>= \mx ->
        mapH_ fill [mn..mx]
      where
        fill i =
                tstH i              >>>= \ok ->
                case ok of
                  True  -> resultis ()
                  False -> zerH i



--
-- Generating Histograms with Mutable Arrays
--


a_histo :: [Int] -> Histo
a_histo l  = runSTArray $
     do arr <- newArray (minMax l) 0
        mapM_ (incST arr) l
        return arr

incST :: STArray s Int Int -> Int -> ST s ()
incST a i =
     do n <- readArray a i
        writeArray a i (n+1)



--
-- The HProg Monad
--


-- PRE: i/p map must be filled (i.e., total between min and max keys)

go :: HProg () -> Histo
go = map2histo . chk . run
      where
        chk (h,Right _) = h
        chk (_,Left er) = error er

mapH_ :: (a->HProg ()) -> [a] -> HProg ()
mapH_ _ []     = resultis ()
mapH_ f (x:xs) = f x >>> mapH_ f xs


newtype HProg a = HP (Map Int Int -> (Map Int Int,Either String a))


run :: HProg a -> (Map Int Int,Either String a)
run (HP hp) = hp Map.empty

resultis :: a -> HProg a
resultis x = HP $ \h -> (h,Right x)

wrong :: String -> HProg a
wrong err = HP $ \h -> (h,Left err)

handle :: HProg a -> HProg a -> HProg a
handle (HP hp) (HP hp') = HP hp''
      where
        hp'' h = case hp h of
                   (h',Right x) -> (h',Right x)
                   (h',Left  _) -> hp' h' 

(>>>) :: HProg () -> HProg a -> HProg a
(>>>) hpH hpH' = hpH >>>= \_ -> hpH'

(>>>=) :: HProg a -> (a->HProg b) -> HProg b
(>>>=) (HP hp) f = HP hp''
      where
        hp'' h = case hp h of
                   (h',Left err) -> (h',Left err)
                   (h',Right x ) -> case f x of
                                      HP hp' -> hp' h'

incH :: Int -> HProg ()
incH i = handle (p_incH i) (zerH i >>> p_incH i)

p_incH :: Int -> HProg ()
p_incH i = HP hp
      where
        hp h = case Map.lookup i h of
                 Nothing -> (                   h,Left "histo elt not inited")
                 Just n  -> (Map.insert i (n+1) h,Right ()                   )

inc0H :: Int -> HProg ()
inc0H i = HP $ \mp -> (Map.insertWith (+) i 1 mp,Right ())

zerH :: Int -> HProg ()
zerH i = HP $ \mp -> (Map.insert i 0 mp,Right ())

minH, maxH :: HProg Int
minH = HP $ \h -> (h,if Map.null h then Left "minH" else Right $ fst $ findMin h)
maxH = HP $ \h -> (h,if Map.null h then Left "maxH" else Right $ fst $ findMax h)

tstH :: Int -> HProg Bool
tstH i = HP $ \h ->(h,Right $ member i h) 



--
-- The Toolbox
--


-- NB: the map must be total between the minimumm and maximum keys

map2histo :: Map Int Int -> Histo
map2histo mp | Map.null mp = array (0,-1) []
map2histo mp | otherwise   = array (mn,mx) $ Map.toList mp
      where
        (mn,_) = findMin mp
        (mx,_) = findMax mp

-- A simple reads wrapper: used here for paring Ints

parse :: Read a => String -> Maybe a
parse str =
    case [ x | (x,t)<-reads str, ("","")<-lex t ] of
      [x] -> Just x
      _   -> Nothing

-- Sorting and partitioning a list  

groupSort :: Ord a => [a] -> [[a]]
groupSort = group . sort

-- minMax (NB: returns (0.-1) for []; model behaviour, not optimized)

minMax :: (Ord a,Num a) => [a] -> (a,a)
minMax [] = (0,-1)
minMax xs = (minimum xs,maximum xs)
