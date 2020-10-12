{-# LANGUAGE RankNTypes, FlexibleInstances, GADTs, KindSignatures #-}
module Graphics.UI.HamGui.Types where

import qualified Data.Vector.Storable.Mutable as MV
import qualified Data.Map.Strict as M
import Control.Monad.State.Strict
import Foreign.C.Types
import Control.Lens

import Graphics.UI.HamGui.BitMapFont

data ScreenPositionTotal = SPT !Float !Float
  deriving Show
$(makePrisms 'SPT)
data ScreenPositionProjected = SPP !Int !Int
  deriving Show
$(makePrisms 'SPP)
data UVCoordinate = UVC !Float !Float
  deriving Show
data RGBColor = RGBC !Float !Float !Float
  deriving Show

newtype ObjectId = ObjectId String deriving (Eq, Ord, Show)

data UIState = Inert | MouseHover ScreenPositionProjected | MouseHeld ScreenPositionProjected deriving Show
$(makePrisms ''UIState)

class Slidable a where
  slideBetween :: Int -> Int -> Int -> a -> a -> a
  fractionBetween :: a -> a -> a -> Float

instance Slidable Float where
  slideBetween lower_bound higher_bound cursor min max = ratioa + min
    where size = higher_bound - lower_bound
          sizea = max - min
          v = cursor - lower_bound
          ratio = (fromIntegral v) / (fromIntegral size)
          ratioa = ratio * sizea
  fractionBetween val_min val_max val = val / (val_max - val_min)

data ObjectState where
  SButton :: ObjectState
  STextLabel :: ObjectState
  STextInput :: String -> ObjectState
  SCheckBox :: Bool -> ObjectState
  SSlider :: Slidable s => s -> Float -> ObjectState
    -- deriving Show
$(makePrisms ''ObjectState)

data Object = Object {
    _boxBox :: (ScreenPositionProjected, ScreenPositionProjected),
    _objectState :: UIState,
    _privateState :: ObjectState
  }
$(makeLenses ''Object)

data Input = Input {
    _mousePos :: Maybe ScreenPositionProjected,
    _mouseKeyState :: Maybe (Bool, Bool),
    _alphaNumPressed :: Maybe String
  }
$(makeLenses ''Input)

data HamGuiData = HamGuiData {
    _vertexDataL :: MV.IOVector CFloat,
    _elemDataL :: MV.IOVector CInt,
    _vI :: CInt,
    _eI :: CInt,
    _vertId :: CInt,
    _screenSize :: ScreenPositionProjected,
    _objectData :: M.Map ObjectId Object,
    _inputs :: Input,
    _cursorPosition :: ScreenPositionProjected,
    _bitMapFont :: BitMapFont,
    _focusedObject :: Maybe ObjectId
  }
$(makeLenses ''HamGuiData)

type HamGui a = StateT HamGuiData IO a