{-# LANGUAGE TemplateHaskell #-}
module Graphics.UI.HamGui.BitMapFont where

import qualified Data.Vector.Storable as V
import qualified Data.Map             as M
import Control.Lens
import Data.Word

data CharacterDefinition = CharDef {
    _charX :: Float,
    _charY :: Float,
    _charSixeX :: Float,
    _charSizeY :: Float,
    _charPenOffsetX :: Float,
    _charPenOffsetY :: Float,
    _charPenAdvanceX :: Float,
    _charPenAdvanceY :: Float
  } deriving (Show, Eq)
$(makeLenses ''CharacterDefinition)

data BitMapFont = BitMapFont {
    _charSet :: M.Map Char CharacterDefinition,
    _squareSide :: Int,
    _rgbaData :: V.Vector Word8,
    _debug :: String
  } deriving Show
$(makeLenses ''BitMapFont)

emptyFont = BitMapFont M.empty 0 V.empty []