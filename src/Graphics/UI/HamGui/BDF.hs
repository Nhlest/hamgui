module Graphics.UI.HamGui.BDF where

import qualified Data.Text.IO         as Text
import qualified Data.Text            as Text
import qualified Data.Vector.Storable as V
import qualified Data.Map             as M
import Text.ParserCombinators.Parsec
import Data.Bits
import Data.List
import Data.Word
import Numeric

import Graphics.UI.HamGui.BitMapFont

data Property = Property String String deriving Show
data Character = Character {
    _charCode   :: Int,
    _charSwidth :: (Int, Int),
    _charDwidth :: (Int, Int),
    _charBbx    :: (Int, Int, Int, Int),
    _charBitmap :: [Int]
  } deriving (Show, Eq) -- TODO: Move to Types maybe? (Or BMF.Types (!))

bdfParser :: GenParser Char st (Int, Int, [Character])
bdfParser = do
  string "STARTFONT 2.1\n"
  manyTill (anyChar) $ try $ string "FONTBOUNDINGBOX "
  many digitOrMinus
  many space
  by <- read <$> many digitOrMinus
  many space
  many digitOrMinus
  many space
  oy <- read <$> many digitOrMinus
  manyTill (anyChar) $ try $ string "STARTPROPERTIES"
  manyTill (anyChar) $ try newline
  _ <- manyTill propParser $ try $ string "ENDPROPERTIES\n"
  string "CHARS "
  manyTill (anyChar) $ try newline
  chars <- manyTill charParser $ try $ string "ENDFONT\n"
  pure (by, oy, chars)
 where propParser = do
         head <$> sepBy (do
                  pname <- manyTill (anyChar) $ try (char ' ')
                  prop <- manyTill (anyChar) $ try newline
                  pure $ Property pname prop
               ) newline
       charParser = do
         string "STARTCHAR "
         _ <- manyTill (anyChar) $ try newline
         string "ENCODING "
         encoding <- numberEndingWith newline
         string "SWIDTH "
         swidth_x <- numberEndingWith space
         swidth_y <- numberEndingWith newline
         string "DWIDTH "
         dwidth_x <- numberEndingWith space
         dwidth_y <- numberEndingWith newline
         string "BBX "
         bbx_w <- numberEndingWith space
         bbx_h <- numberEndingWith space
         bbx_x <- numberMinusEndingWith space
         bbx_y <- numberMinusEndingWith newline
         string "BITMAP\n"
         bitmap_rows <- map ((flip shiftR) (8-(bbx_w `mod` 8))) <$> map (fst . head) <$> (map readHex) <$>
                          (manyTill (manyTill hexDigit newline) $ try $ string "ENDCHAR\n")
         pure $ Character encoding (swidth_x, swidth_y) (dwidth_x, dwidth_y) (bbx_w, bbx_h, bbx_x, bbx_y) bitmap_rows
       digitOrMinus = oneOf "1234567890-"
       numberEndingWith a = read <$> (manyTill (digit) $ try a)
       numberMinusEndingWith a = read <$> (manyTill (digitOrMinus) $ try a)

fillTillSide storySoFar squareSide currentRowHeight =
  let (before, current) = splitAt ((length storySoFar) - currentRowHeight) storySoFar in
    before ++ [r++(replicate (squareSide - (length r)) 0)|r <- current]

-- TODO: refactor
tryToFit :: Int -> [Character] -> Maybe ([[Word8]], M.Map Char CharacterDefinition)
tryToFit squareSide_p chars_p@((Character _ _ _ (_, bh, _, _) _):_) =
  go (M.empty) squareSide_p chars_p (replicate bh []) 0 bh
  where go characterMap squareSide [] storySoFar _ currentRowHeight = Just $ (fillTillSide storySoFar squareSide currentRowHeight, characterMap)
        go characterMap squareSide chars@((Character ch (_sx, _sy) (dx, dy) (bw, bh, bx, by) bm):xs) storySoFar currentWidthCursor currentRowHeight =
          if length storySoFar > squareSide then tryToFit (squareSide_p * 2) chars_p else
          if currentWidthCursor+bw >= squareSide then go characterMap squareSide chars ((fillTillSide storySoFar squareSide currentRowHeight) ++ (replicate bh [])) 0 bh else
          let (before, current) = splitAt ((length storySoFar) - currentRowHeight) storySoFar in
            go (M.insert (toEnum ch)
            (let ss = fromIntegral squareSide
                 cx = fromIntegral currentWidthCursor
                 cy = fromIntegral $ length before + (currentRowHeight - bh)
                 sx = fromIntegral bw
                 sy = fromIntegral bh
             in CharDef (cx/ss) (cy/ss) (sx/ss) (sy/ss) sx sy
                        (fromIntegral bx) (fromIntegral by)
                        (fromIntegral dx) (fromIntegral dy)) characterMap)
             squareSide xs (before ++ [let row = current !! ix in row ++ [get bm c ix currentRowHeight bw | c <- [0..bw-1]] | ix <- [0..length current-1]]) (currentWidthCursor+bw) currentRowHeight
        get bm x y h w = if y < (h-char_height) then skip else
          if testBit (bm !! (y-(h-char_height))) (w-x-1) then fill else empty
          where char_height = length bm
                empty = 0::Word8
                skip = 128::Word8
                fill  = 255::Word8

loadBDF :: FilePath -> IO (BitMapFont)
loadBDF path = do
  bdfImage <- Text.unpack <$> Text.readFile path
  let result = parse bdfParser "" bdfImage
  (bounding_height, bounding_offset, chars) <- case result of
    Left a -> print a >> fail "pepega"
    Right b -> pure b
  let sortedChars = sortBy (\(Character _ (_, _) (_, _) (_, bh, _, _)  _)
                             (Character _ (_, _) (_, _) (_, bh2, _, _) _) -> compare bh2 bh) chars
  let Just (p, cset) = tryToFit 2 sortedChars
  let side = length $ head p
  let concatenate = let c = concat p in c ++ (replicate ((side * side) - length c) 0)
  pure $ BitMapFont {
      _charSet = cset,
      _squareSide = side,
      _boundingHeigh = bounding_height,
      _boundingOffset = bounding_offset,
      _rgbaData = V.fromList $ concatenate
    }