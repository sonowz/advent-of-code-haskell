module Y2021.Day16 where

import Lib.IO
import Lib.Types
import Relude
import Relude.Extra.Bifunctor
import Relude.Extra.Foldable1
import Relude.Extra.Map
import Relude.Extra.Newtype
import Relude.Extra.Tuple
import qualified Relude.Unsafe as Unsafe
import Text.Parsec.Combinator
import Text.Parsec.Pos (SourcePos, incSourceColumn, sourceColumn)
import Text.Parsec.Prim hiding ((<|>), many)
import qualified Text.Show as S

-----------------------
-- Type declarations --
-----------------------

data Binary = Zero | One deriving (Eq, Ord, Show)

-- Denotes just a stream of binaries
type BinStream = [Binary]

-- Denotes a number represented as binaries
newtype BinNumber = BN [Binary] deriving (Semigroup, Monoid) via [Binary]

data Packet
    = LiteralPacket Version BinNumber
    | OperatorPacket Version PacketOp [Packet]
    deriving (Show)
newtype Version = Version Int deriving (Show)
newtype PacketID = PacketId Int deriving (Show)
data PacketOp = OpSum | OpProduct | OpMin | OpMax | OpGT | OpLT | OpEQ deriving (Show)

-- This solution uses 'parsec' library
-- in order to parse 'BinStream' (= '[Binary]') type into 'Packet' type
type Parser' = Parsec BinStream ()

type HexNum = Text

instance S.Show BinNumber where
    -- This also shows number as integer
    show n = "(" <> intForm n <> ") " <> binForm (un n)      where
        intForm = show . binToInt
        binForm []          = ""
        binForm (Zero : bs) = '0' : binForm bs
        binForm (One  : bs) = '1' : binForm bs

binToInt :: Num a => BinNumber -> a
binToInt num = go (un num) 0  where
    go []          acc = acc
    go [Zero     ] acc = acc
    go [One      ] acc = acc + 1
    go (Zero : bs) acc = go bs (2 * acc)
    go (One  : bs) acc = go bs (2 * (acc + 1))

intToBin :: Integral a => a -> BinNumber
intToBin num = BN (go num [])  where
    go 0 acc = Zero : acc
    go 1 acc = One : acc
    go n acc = case n `divMod` 2 of
        (q, 0) -> go q (Zero : acc)
        (q, 1) -> go q (One : acc)
        _      -> error "Impossible!"

hexToBin :: HexNum -> BinStream
hexToBin = toString >=> parseBin  where
    parseBin :: Char -> BinStream
    parseBin 'A' = un $ intToBin 10
    parseBin 'B' = un $ intToBin 11
    parseBin 'C' = un $ intToBin 12
    parseBin 'D' = un $ intToBin 13
    parseBin 'E' = un $ intToBin 14
    parseBin 'F' = un $ intToBin 15
    parseBin c   = pad4 . un . intToBin . readInt . toText $ ([c] :: String)
        where pad4 l = replicate (4 - length l) Zero <> l

intToOp :: Int -> PacketOp
intToOp 0 = OpSum
intToOp 1 = OpProduct
intToOp 2 = OpMin
intToOp 3 = OpMax
intToOp 5 = OpGT
intToOp 6 = OpLT
intToOp 7 = OpEQ
intToOp _ = error "Invalid operator!"

------------
-- Part 1 --
------------

solve1 :: BinStream -> Int
solve1 = sumVersionNumbers . parsePacket

parsePacket :: BinStream -> Packet
parsePacket packetStream = fromRight (error "parse error") $ parse pPacketAligned "" packetStream

sumVersionNumbers :: Packet -> Int
sumVersionNumbers (LiteralPacket (Version v) _) = v
sumVersionNumbers (OperatorPacket (Version v) _ subpackets) =
    v + sum (sumVersionNumbers <$> subpackets)


-- Primitive parsers --

_incPos :: SourcePos -> Binary -> BinStream -> SourcePos
_incPos pos _ _ = incSourceColumn pos 1

pBinary :: Parser' Binary
pBinary = tokenPrim show _incPos Just

pZero :: Parser' Binary
pZero = tokenPrim show _incPos (guarded (== Zero))

pOne :: Parser' Binary
pOne = tokenPrim show _incPos (guarded (== One))

pBinStream :: Int -> Parser' BinStream
pBinStream n = do
    bins <- count n pBinary
    guard (length bins == n)
    return bins

pBinNumber :: Int -> Parser' BinNumber
pBinNumber = fmap BN . pBinStream


-- Composite parsers --

-- 4-bit aligned packet
pPacketAligned :: Parser' Packet
pPacketAligned = do
    packet <- pPacket
    column <- sourceColumn <$> getPosition :: Parser' Int
    let padding = (column - 1) `mod` 4
    replicateM_ padding pZero
    return packet

-- Not aligned packet
pPacket :: Parser' Packet
pPacket = do
    version    <- Version . binToInt <$> pBinNumber 3
    packetType <- binToInt <$> pBinNumber 3 :: Parser' Int
    if packetType == 4 then pLiteralPacket version else pOperatorPacket version packetType

pLiteralPacket :: Version -> Parser' Packet
pLiteralPacket version = LiteralPacket version . BN <$> pValue  where
    pValue :: Parser' BinStream
    pValue = pBinStream 5 >>= \case
        Zero : value -> return value
        One  : value -> (<>) value <$> pValue
        []           -> fail "Impossible!"

pOperatorPacket :: Version -> Int -> Parser' Packet
pOperatorPacket version packetType = do
    subpackets <- try pTotalLength <|> try pSubpacketLength
    return (OperatorPacket version (intToOp packetType) subpackets)
  where
    pTotalLength = do
        pZero
        length      <- binToInt <$> pBinNumber 15
        startColumn <- getColumn
        whileM ((\endColumn -> endColumn < startColumn + length) <$> getColumn) pPacket
    pSubpacketLength = do
        pOne
        count <- binToInt <$> pBinNumber 11
        replicateM count pPacket
    getColumn = sourceColumn <$> getPosition :: Parser' Int
    whileM :: Monad m => m Bool -> m a -> m [a]
    whileM p action =
        p >>= \b -> if b then action >>= \x -> fmap (x :) (whileM p action) else return []

------------
-- Part 2 --
------------

solve2 :: BinStream -> Integer
solve2 = calcValue . parsePacket

calcValue :: Packet -> Integer
calcValue (LiteralPacket _ value         ) = binToInt value
calcValue (OperatorPacket _ op subpackets) = runOp op (calcValue <$> subpackets)

runOp :: PacketOp -> [Integer] -> Integer
runOp OpSum     l        = sum l
runOp OpProduct l        = product l
runOp OpMin     l        = fromMaybe 0 . viaNonEmpty minimum1 $ l
runOp OpMax     l        = fromMaybe 0 . viaNonEmpty maximum1 $ l
runOp OpGT      [x1, x2] = if x1 > x2 then 1 else 0
runOp OpLT      [x1, x2] = if x1 < x2 then 1 else 0
runOp OpEQ      [x1, x2] = if x1 == x2 then 1 else 0
runOp OpGT      _        = error "Undefined behavior!"
runOp OpLT      _        = error "Undefined behavior!"
runOp OpEQ      _        = error "Undefined behavior!"

--------------------
-- Main & Parsing --
--------------------

main' :: IO ()
main' = do
    rawPacket <-
        parseRawPacket . (Unsafe.!! 0) <$> readFileLines "inputs/Y2021/Day16.txt" :: IO BinStream
    print $ solve1 rawPacket
    print $ solve2 rawPacket

parseRawPacket :: Text -> BinStream
parseRawPacket = hexToBin
