module Y2023.Day24 (main') where

import Data.Ratio ((%))
import Data.Text qualified as T
import Lib.IO
import Relude
import Relude.Unsafe qualified as Unsafe

-----------------------
-- Type declarations --
-----------------------

data Hail = Hail
  { pos :: Pos,
    vel :: Velocity
  }
  deriving (Show, Eq, Ord)

data Pos = Pos Integer Integer Integer deriving (Show, Eq, Ord)

data Velocity = Velocity Integer Integer Integer deriving (Show, Eq, Ord)

------------
-- Part 1 --
------------

solve1 :: [Hail] -> Int
solve1 hails = length validCollisionPoints
  where
    hailPairs = [(h1, h2) | h1 <- hails, h2 <- hails, h1 < h2]
    collisionPoints = uncurry calcLineCollisionPointXY <$> hailPairs :: [Maybe (Rational, Rational)]
    validCollisionPoints = filter validateFn (zip hailPairs collisionPoints)
    validateFn (hailPair, cp) = case cp of
      Just collisionPoint -> bimapPred isInsideTestArea collisionPoint && bimapPred (`collidesInFuture` collisionPoint) hailPair
      Nothing -> False

calcLineCollisionPointXY :: Hail -> Hail -> Maybe (Rational, Rational)
calcLineCollisionPointXY (Hail (Pos px1 py1 _) (Velocity vx1 vy1 _)) (Hail (Pos px2 py2 _) (Velocity vx2 vy2 _)) = collisionPoint
  where
    denominator = vx1 * vy2 - vx2 * vy1
    x = (vx1 * vy2 * px2 - vx2 * vx1 * py2 + vx2 * vx1 * py1 - vx2 * vy1 * px1) % denominator
    y = (vx1 * vy2 * py1 - vy1 * vx2 * py2 - vy1 * vy2 * px1 + vy1 * vy2 * px2) % denominator
    collisionPoint = if denominator == 0 then Nothing else Just (x, y)

isInsideTestArea :: Rational -> Bool
isInsideTestArea x = x >= 200_000_000_000_000 && x <= 400_000_000_000_000

collidesInFuture :: Hail -> (Rational, Rational) -> Bool
collidesInFuture (Hail (Pos px _ _) (Velocity vx _ _)) (cx, _) = (cx - fromIntegral px) * signum (fromIntegral vx) > 0

bimapPred :: (a -> Bool) -> (a, a) -> Bool
bimapPred f (x, y) = f x && f y

------------
-- Part 2 --
------------

solve2 :: [Hail] -> Integer
solve2 hails = x + y + z
  where
    velocityRanges = [-500 .. 500] :: [Integer] -- Assume velocity is in this range
    velocities = [Velocity vx vy vz | vx <- velocityRanges, vy <- velocityRanges, vz <- velocityRanges]
    rockPosition = Unsafe.head $ mapMaybe (calcRockPos hails) velocities
    Pos x y z = rockPosition

calcRockPos :: [Hail] -> Velocity -> Maybe Pos
calcRockPos hails rockVelocity = do
  let (hail1 : hail2 : _) = hails
  (x, y) <- calcRockPosXY hail1 hail2 rockVelocity
  z <- calcRockPosZ hail1 rockVelocity x
  let rockPos = Pos x y z
  guard $ all (\hail -> collidesInFuture2 hail rockPos rockVelocity) hails
  pure rockPos

calcRockPosXY :: Hail -> Hail -> Velocity -> Maybe (Integer, Integer)
calcRockPosXY (Hail (Pos px1 py1 _) (Velocity vx1 vy1 _)) (Hail (Pos px2 py2 _) (Velocity vx2 vy2 _)) (Velocity vx' vy' _) = do
  let a = vy' - vy1
      b = vx' - vx1
      c = (vy' - vy1) * px1 - (vx' - vx1) * py1
      d = vy' - vy2
      e = vx' - vx2
      f = (vy' - vy2) * px2 - (vx' - vx2) * py2
      denominator = a * e - b * d
  guard $ denominator /= 0
  guard $ (c * e - b * f) `mod` denominator == 0
  guard $ (c * d - a * f) `mod` denominator == 0
  let px' = (c * e - b * f) `div` denominator
      py' = (c * d - a * f) `div` denominator
  pure (px', py')

calcRockPosZ :: Hail -> Velocity -> Integer -> Maybe Integer
calcRockPosZ (Hail (Pos px _ pz) (Velocity vx _ vz)) (Velocity vx' _ vz') px' = do
  let denominator = vx' - vx
  guard $ denominator /= 0
  let pz' = pz + (vz' - vz) * (px' - px) `div` denominator
  pure pz'

data CollisionTime = CollidesIn Integer | Never | SameVelocity deriving (Show, Eq)

collidesInFuture2 :: Hail -> Pos -> Velocity -> Bool
collidesInFuture2 (Hail (Pos px py pz) (Velocity vx vy vz)) (Pos px' py' pz') (Velocity vx' vy' vz') = collides
  where
    getCollisionTime :: Integer -> Integer -> Integer -> Integer -> CollisionTime
    getCollisionTime p v p' v' | v == v' && p == p' = SameVelocity
    getCollisionTime p v p' v' | v == v' && p /= p' = Never
    getCollisionTime p v p' v' | (p - p') `mod` (v - v') /= 0 = Never
    getCollisionTime p v p' v' =
      let collisionTime = (p' - p) `div` (v - v')
       in if collisionTime < 0 then Never else CollidesIn collisionTime

    collisionTimes = [getCollisionTime px vx px' vx', getCollisionTime py vy py' vy', getCollisionTime pz vz pz' vz'] :: [CollisionTime]
    collides = noNever && sameCollisionTime
      where
        noNever = Never `notElem` collisionTimes
        colTimes = mapMaybe (\case CollidesIn t -> Just t; _ -> Nothing) collisionTimes
        sameCollisionTime = length (ordNub colTimes) <= 1

--------------------
-- Main & Parsing --
--------------------

main' :: IO ()
main' = do
  hails <- parseHail <<$>> readFileLines "inputs/Y2023/Day24.txt" :: IO [Hail]
  print $ solve1 hails
  print $ solve2 hails

parseHail :: Text -> Hail
parseHail line = Hail (Pos posX posY posZ) (Velocity velX velY velZ)
  where
    dropped = T.filter (\c -> c /= ',' && c /= '@') line
    [posX, posY, posZ, _, velX, velY, velZ] = fromIntegral . readInt <$> T.splitOn " " dropped
