{-# LANGUAGE RankNTypes, FlexibleInstances, GADTs, KindSignatures #-}
module Graphics.UI.HamGui.Types where

import qualified Data.Vector.Storable.Mutable as MV
import qualified Data.Map.Strict as M
import Control.Monad.State.Lazy
import Foreign.C.Types
import Control.Lens
import Type.Reflection

import Graphics.UI.HamGui.BitMapFont

data ScreenPosition = SP !Int !Int
$(makePrisms 'SP)
data ScreenSize = SS !Int !Int
$(makePrisms 'SS)
data ScreenRect = SRect !ScreenPosition !ScreenSize
$(makePrisms 'SRect)
data ScreenPositionNormalized = SPN !Float !Float
$(makePrisms 'SPN)
data ScreenSizeNormalized = SSN !Float !Float
$(makePrisms 'SSN)
data ScreenRectNormalized = SRNect !ScreenPositionNormalized !ScreenSizeNormalized
$(makePrisms 'SRNect)
data UVCoordinate = UVC !Float !Float
$(makePrisms 'UVC)
data RGBColor = RGBC !Float !Float !Float
$(makePrisms 'RGBC)

newtype ObjectId = ObjectId String deriving (Eq, Ord)

newtype WindowId = WindowId String deriving (Eq, Ord)

data UIState = Inert | MouseHover ScreenPosition | MouseHeld ScreenPosition
$(makePrisms ''UIState)

class Slidable a where
  -- | slideBetween screenPosition_x screenPosition_x2 screenPosition_actual val_min val_max -> val
  slideBetween    :: Int -> Int -> Int -> a -> a -> a
  -- | slideBetween val_min val_max val -> fraction
  fractionBetween :: a -> a -> a -> Float

instance Slidable Float where
  slideBetween lower_bound higher_bound cursor min max = ratioa + min
    where size   = higher_bound - lower_bound
          sizea  = max - min
          v      = cursor - lower_bound
          ratio  = (fromIntegral v) / (fromIntegral size)
          ratioa = ratio * sizea
  fractionBetween val_min val_max val = val / (val_max - val_min)

instance Slidable Int where
  slideBetween lower_bound higher_bound cursor min max = ratioa + min
    where size   = higher_bound - lower_bound
          sizea  = max - min
          v      = cursor - lower_bound
          ratio  = (fromIntegral v) / (fromIntegral size)
          ratioa = floor $ ratio * fromIntegral sizea
  fractionBetween val_min val_max val = (fromIntegral val) / (fromIntegral (val_max - val_min))

slideBetweenClamped :: Slidable a => Int -> Int -> Int -> a -> a -> a
slideBetweenClamped a b c m x = slideBetween a b cclam m x
  where cclam = if c > b then b else
                if c < a then a else c

data ObjectState where
  SButton    :: ObjectState
  STextLabel :: ObjectState
  STextInput :: String -> ObjectState
  SCheckBox  :: Bool -> ObjectState
  SSlider    :: (Slidable s, Typeable s, Show s) => s -> ObjectState
$(makePrisms ''ObjectState)

data Object = Object {
    _boxBox :: ScreenRect,
    _generalObjectState :: UIState,
    _specificObjectState :: ObjectState
  }
$(makeLenses ''Object)

data MouseKeyState = MKDown | MKUp deriving (Eq)

data Input = Input {
    _mousePos :: Maybe ScreenPosition,
    _mouseKeyState :: Maybe (MouseKeyState, MouseKeyState),
    _alphaNumPressed :: Maybe String
  }
$(makeLenses ''Input)

data HamGuiData = HamGuiData {
    _vertexDataL    :: MV.IOVector CFloat,
    _elemDataL      :: MV.IOVector CInt,
    _vI             :: CInt,
    _eI             :: CInt,
    _vertId         :: CInt,
    _screenSize     :: ScreenSize,
    _objectData     :: M.Map ObjectId Object,
    _inputs         :: Input,
    _cursorPosition :: ScreenPosition,
    _bitMapFont     :: BitMapFont,
    _focusedObject  :: Maybe ObjectId,
    _heldObject     :: Maybe ObjectId
  }
$(makeLenses ''HamGuiData)

typeOf' :: forall a. (Slidable a, Typeable a, Show a) => a -> TypeRep a
typeOf' _ = typeRep::(TypeRep a)

type HamGui a = StateT HamGuiData IO a

emptyInputs :: Input
emptyInputs = Input Nothing Nothing Nothing