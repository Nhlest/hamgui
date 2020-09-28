{-# LANGUAGE TemplateHaskell #-}
module Graphics.UI.HamGui.BitMapFont where

import Data.Vector.Storable
import Control.Lens
import Data.Word
import Data.Map

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