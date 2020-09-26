{-# LANGUAGE TemplateHaskell #-}
module Graphics.UI.BitMapFont where

import Data.Map
import Data.Word
import Data.Vector.Storable
import Control.Lens

data CharacterDefinition = CharDef {
    _charX :: Float,
    _charY :: Float,
    _charSixeX :: Float,
    _charSizeY :: Float,
    _charPenOffsetX :: Float,
    _charPenOffsetY :: Float,
    _charPenAdvanceX :: Float,
    _charPenAdvanceY :: Float
  } deriving Show
$(makeLenses ''CharacterDefinition)

data BitMapFont = BitMapFont {
    _charSet :: Map Char CharacterDefinition,
    _squareSide :: Int,
    _rgbaData :: Vector Word8,
    _debug :: String
  } deriving Show
$(makeLenses ''BitMapFont)