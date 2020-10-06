module Graphics.UI.HamGui.Types where

import qualified Data.Map.Lazy as M
import Control.Monad.State.Lazy
import Foreign.C.Types
import Control.Lens

import Graphics.UI.HamGui.BitMapFont

type ScreenPositionTotal = (Float, Float)
type ScreenPositionProjected = (Int, Int)

newtype ObjectId = ObjectId String deriving (Eq, Ord, Show)

data UIState = Inert | MouseHover ScreenPositionProjected | MouseHeld ScreenPositionProjected deriving Show
$(makePrisms ''UIState)

data ObjectState =
    SButton
  | STextInput String
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

data HamGuiData = HamGuiData {
    _vertexDataL :: [CFloat],
    _elemDataL :: [CInt],
    _vertId :: CInt,
    _screenSize :: ScreenPositionProjected,
    _objectData :: M.Map ObjectId Object,
    _inputs :: Input,
    _cursorPosition :: (Int, Int),
    _bitMapFont :: BitMapFont,
    _focusedObject :: Maybe ObjectId
  }
$(makeLenses ''HamGuiData)

type HamGui a = StateT HamGuiData IO a