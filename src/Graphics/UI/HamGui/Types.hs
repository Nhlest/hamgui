module Graphics.UI.HamGui.Types where

import qualified Data.Map.Lazy as M
import Control.Monad.State.Lazy
import Foreign.C.Types
import Control.Lens

type ScreenPositionTotal = (Float, Float)
type ScreenPositionProjected = (Int, Int)

newtype ObjectId = ObjectId String deriving (Eq, Ord, Show)

data UIState = Inert | MouseHover ScreenPositionProjected | MouseHeld ScreenPositionProjected deriving Show
$(makePrisms ''UIState)

data Object = Object {
    _boxBox :: (ScreenPositionProjected, ScreenPositionProjected),
    _objectState :: UIState
  } deriving Show
$(makeLenses ''Object)

data Input = Input {
    _mousePos :: Maybe ScreenPositionProjected,
    _mouseKeyState :: Maybe (Bool, Bool)
  }
$(makeLenses ''Input)

data HamGuiData = HamGuiData {
    _vertexDataL :: [CFloat],
    _elemDataL :: [CInt],
    _vertId :: CInt,
    _screenSize :: ScreenPositionProjected,
    _objectData :: M.Map ObjectId Object,
    _inputs :: Input,
    _cursorPosition :: (Int, Int)
  }
$(makeLenses ''HamGuiData)

type HamGui a = StateT HamGuiData IO a