{-# LANGUAGE RankNTypes #-}
module Graphics.UI.HamGui.Types where

import qualified Data.Vector.Storable.Mutable as MV
import qualified Data.Map.Strict as M
import Control.Monad.State.Strict
import Foreign.C.Types
import Control.Lens

import Graphics.UI.HamGui.BitMapFont

data ScreenPositionTotal = SPT !Float !Float
  deriving Show
data ScreenPositionProjected = SPP !Int !Int
  deriving Show
data UVCoordinate = UVC !Float !Float
  deriving Show
data RGBColor = RGBC !Float !Float !Float
  deriving Show

newtype ObjectId = ObjectId String deriving (Eq, Ord, Show)

data UIState = Inert | MouseHover ScreenPositionProjected | MouseHeld ScreenPositionProjected deriving Show
$(makePrisms ''UIState)

data ObjectState =
    SButton
  | STextLabel
  | STextInput String
  | SCheckBox Bool
  deriving Show
$(makePrisms ''ObjectState)

data Object = Object {
    _boxBox :: (ScreenPositionProjected, ScreenPositionProjected),
    _objectState :: UIState,
    _privateState :: ObjectState
  } deriving Show
$(makeLenses ''Object)

data Input = Input {
    _mousePos :: Maybe ScreenPositionProjected,
    _mouseKeyState :: Maybe (Bool, Bool),
    _alphaNumPressed :: Maybe String
  }
$(makeLenses ''Input)

data HamGuiData u = HamGuiData {
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
    _focusedObject :: Maybe ObjectId,
    _userData :: u
  }
$(makeLenses ''HamGuiData)

type HamGuiU u a = StateT (HamGuiData u) IO a
type HamGui a = forall u. HamGuiU u a