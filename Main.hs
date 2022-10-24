import Control.Monad
import qualified System.Random as Rand
import qualified System.Environment as Env
import qualified Data.List as List
import Data.Array

data State a = Bomb | Unexplored | Info a deriving (Eq, Show)

instance Functor State where
  fmap f Bomb = Bomb
  fmap f Unexplored = Unexplored
  fmap f (Info x) = Info (f x)


type Point = (Int, Int)
type World n m = Array (n,m) (State Int)
type Explored n m = Array (n,m) (State Int)

main :: IO ()
main = do
  args <- Env.getArgs
  let [m, n, bombs] = map (\x -> read x::Int) args
  g <-Rand.getStdGen
  let explored = makeMatrix m n Unexplored
  let world = makeGame m n bombs g
  runGame explored world >>= showWorld



makeMatrix :: Int -> Int -> a -> Array (Int, Int) a
makeMatrix n m x = array ((1,n),(1,m)) [((i,j),x) | i <- [1..n], j <- [1..m]]

runGame :: Explored m n -> World m n-> IO (Explored m n)
runGame e w = do
  showWorld e
  move <- getLine
  let newE = explore w (parseInput move) e
  if isBomb newE then return  newE else runGame newE w


  {- Generates a world of dimensions w, h, with n mines scattered
   randomly throughout and all the clues numbers calculated -}
genGame :: Rand.RandomGen g => Int -> Int -> Int -> g -> World Int Int
genGame n m bomb_cnt g = game
  where
    mines   = List.nub $ genPoints n m bomb_cnt g
    world = genWorld n m bomb_cnt mines
    infoMap = genInfoMap n m world
    game = array ((1,n),(1,m)) (zipWith (\(i, x) (_, y)-> (i, combine (+) x y)) (assocs world) (assocs infoMap))

zipp f a1 a2 = array (bounds a1) (zipWith f (assocs a1) (assocs a2))

combine :: (a-> b -> c) -> State a -> State b -> State c
combine _ Bomb _ = Bomb
combine _ Unexplored Bomb = Bomb
combine _ Unexplored Unexplored = Unexploreds
combine _ (Info x) Bomb = Bomb
combine f (Info x) (Info y) = Info (f x y)


genPoints :: Rand.RandomGen g => Int -> Int -> Int -> g -> [Point]
genPoints w h n g = zip xs ys
    where
      xs = take n (Rand.randomRs (0, w-1) g)
      ys = drop n $ take (n*2) $ Rand.randomRs (0, h-1) g

genWorld :: Int -> Int -> Int -> [Point] -> World Int Int
genWorld w h n l = world // bombsAtPoints l
    where
      world = makeMatrix w h (Info 0) -- Initial world has no mines
      bombsAtPoints ps = do
        p <-ps
        return (p, Bomb)

genInfoMap :: Int -> Int -> World Int Int -> World Int Int
genInfoMap n m mines = array ((1,n), (1,m)) [((i,j),Info $ neighbourValues i j mines) | i <-[1..n], j<-[1..m]]



surroundingIndices:: Int -> Int -> Int -> Int -> [(Int, Int)]
surroundingIndices i j n m= case i of
  1 -> case j of
    1 -> [(1,2), (2,1),(2,2)]
    m -> [(1,j-1),(2,j),(2,j-1)]
    _ -> [(1,j-1),(1,j+1),(2,j-1),(2,j),(2,j+1)]
  n -> case j of
    1 -> [(n-1,1), (n, 2),(n-1,2)]
    m -> [(n-1,m), (n, m-1),(n-1,m-1)]
    _ -> [(n-1, j-1),(n-1,j),(n-1,j+1),(n,j-1),(n,j+1)]
  _ -> [(i-1,j-1),(i-1, j), (i-1, j+1), (i, j-1), (i, j+1),(i+1, j-1), (i+1, j), (i+1, j+1)]

neighbourValues :: Int -> Int -> World Int Int -> Int
neighbourValues a b mines = length $ filter (/=Bomb) $ map (mines !) (surroundingIndices a b n m)
  where ((_,n),(_,m)) = bounds mines
