module Graphics.UI.HamGui.HamGui where

import Data.Vector.Storable hiding ((++), forM_)
import Control.Monad.State.Lazy
import qualified Data.Map as M
import Foreign.C.Types
import Control.Lens
import Foreign.Ptr
import Data.Maybe

import Graphics.UI.HamGui.BitMapFont
import Graphics.UI.HamGui.Types

toSPT :: ScreenPositionProjected -> HamGui ScreenPositionTotal
toSPT (x, y) = do
  (sx, sy) <- use screenSize
  pure (fromIntegral x / fromIntegral sx * 2.0 - 1.0, fromIntegral y / fromIntegral sy * 2.0 - 1.0)

toSPTU :: ScreenPositionProjected -> HamGui ScreenPositionTotal
toSPTU (x, y) = do
  (sx, sy) <- use screenSize
  pure (fromIntegral x / fromIntegral sx * 2.0, fromIntegral y / fromIntegral sy * 2.0)

skipUV :: (Float, Float)
skipUV = (-1.0, -1.0)

setScreenSize :: ScreenPositionProjected -> HamGui ()
setScreenSize = assign screenSize

uploadMouseState :: ScreenPositionProjected -> (Bool, Bool) -> HamGui ()
uploadMouseState a b = do
  inputs . mousePos .= Just a
  inputs . mouseKeyState .= Just b

uploadAlphaNums :: String -> HamGui ()
uploadAlphaNums a = do
  inputs . alphaNumPressed .= Just a

addRect :: ScreenPositionTotal -> ScreenPositionTotal -> (Float, Float, Float) -> (Float, Float) -> (Float, Float) -> HamGui ()
addRect (x0, y0) (sx, sy) (r, g, b) (u0, v0) (u1, v1) = do
  addVertex (x0   ) (y0   ) (u0) (v1)
  addVertex (x0+sx) (y0   ) (u1) (v1)
  addVertex (x0+sx) (y0+sy) (u1) (v0)
  addVertex (x0   ) (y0+sy) (u0) (v0)
  addElem (0) (1) (2)
  addElem (0) (2) (3)
  vertId += 4
  pure ()
 where addColor = do
         vertexDataL <<>= [(CFloat i) | i <- [r, g, b]]
       addVertex x y ux uy = do
         vertexDataL <<>= [(CFloat i) | i <- [x, y]]
         addColor
         vertexDataL <<>= [(CFloat ux)]
         vertexDataL <<>= [(CFloat uy)]
       addElem i0 i1 i2 = do
         vertId <- use vertId
         elemDataL <<>= [(i + vertId) | i <- [i0, i1, i2]]

addGlyph :: Char -> ScreenPositionTotal -> (Float, Float) -> HamGui ()
addGlyph ch (x, y) (w, h) = do
  bmf <- use bitMapFont
  let f = bmf ^. charSet
  let char = M.lookup ch f
  case char of
    Nothing -> liftIO $ fail "pepega character not found"
    Just (CharDef cx cy sx sy _ox _oy _ax _ay) -> do
      addRect (x, y) (w, h) (1.0, 0.0, 0.0) (cx, cy) ((cx+sx), (cy+sy))

addText :: String -> ScreenPositionTotal -> (Float, Float) -> HamGui ()
addText str (x, y) (w, h) = do
  bmf <- use bitMapFont
  go bmf str (x, y) (w, h)
 where go _ [] _ _ = pure ()
       go bmf (ch:rest) (x, y) (w, h) = do
         let f = bmf ^. charSet
         let char = M.lookup ch f
         case char of
           Nothing -> liftIO $ fail "pepega character not found"
           Just (CharDef _cx _cy sx sy ox oy ax ay) -> do
             addGlyph ch (x+ox*w, y+oy*h) (sx*w, sy*h)
             go bmf rest (x+ax*w, y+ay*h) (w, h)
         pure ()

addRectWithBorder :: ScreenPositionTotal -> ScreenPositionTotal -> (Float, Float, Float) -> (Float, Float, Float) -> HamGui ()
addRectWithBorder p@(x0, y0) s@(sx, sy) c@(_r, _g, _b) cb@(_rb, _gb, _bb) = do
  addRect p s cb skipUV skipUV
  addRect (x0+0.01, y0+0.01) (sx-0.02, sy-0.02) c skipUV skipUV

initHamGuiData :: HamGuiData
initHamGuiData = HamGuiData [] [] 0 (-900, 0) M.empty (Input Nothing Nothing Nothing) (0, 0) emptyFont Nothing

clearBuffers :: HamGui ()
clearBuffers = do
  vertexDataL    .= []
  elemDataL      .= []
  vertId         .= 0
  cursorPosition .= (512, 512)

composeBuffers :: (Ptr CFloat -> IO ()) -> (Ptr CInt -> IO ()) -> HamGui ()
composeBuffers actionA actionE = do
  dv <- fromList <$> use vertexDataL
  ev <- fromList <$> use elemDataL
  liftIO $ unsafeWith dv actionA
  liftIO $ unsafeWith ev actionE
  pure ()

processInputs :: HamGui ()
processInputs = do
  pure ()

newFrame :: HamGui ()
newFrame = do
  clearBuffers
  processInputs

-- Actual widgetds

checkIfPointIsInsideBox :: ScreenPositionProjected -> (ScreenPositionProjected, ScreenPositionProjected) -> Bool
checkIfPointIsInsideBox (x, y) ((bx, by), (sx, sy)) = do
  x >= bx && x <= bx + sx && y >= by && y <= by + sy

genericObjectInputCheck :: ObjectId -> HamGui Bool
genericObjectInputCheck oId = do
  dataFromLastFrame <- M.lookup oId <$> use objectData
  mouse             <- use $ inputs . mousePos
  mouseM            <- fromJust <$> (use $ inputs . mousePos)
  lmb               <- fromMaybe False <$> preview (_Just . _1) <$> (use $ inputs . mouseKeyState)
  let hoverStatus = fromMaybe False $ do
       obj  <- dataFromLastFrame
       mpos <- mouse
       pure $ checkIfPointIsInsideBox mpos $ obj ^. boxBox
  let clickedStatus = hoverStatus && lmb
  let justClickedStatus = fromMaybe False $ do
        obj <- dataFromLastFrame
        let state = obj ^. objectState
        _ <- state ^? _MouseHeld
        pure $ not lmb
  objectData . at oId . mapped . objectState .= -- TODO: do not transfer hover status if clicked from outside of any widget
    if      clickedStatus && not justClickedStatus  then MouseHeld mouseM
    else if clickedStatus && justClickedStatus      then MouseHeld mouseM
    else if hoverStatus                             then MouseHover mouseM
    else                                                 Inert
  when justClickedStatus $ focusedObject .= Just oId
  pure justClickedStatus

button :: ObjectId -> String -> HamGui Bool
button oId label = do
  clicked <- genericObjectInputCheck oId
  let buttonBoxSize = (200, 50)
  cursorP        <- use cursorPosition
  cursor         <- toSPT cursorP
  buttonBoxSizeT <- toSPTU buttonBoxSize
  isFocused      <- fromMaybe False <$> ((fmap . fmap) ((==) oId) $ use focusedObject)
  cursorPosition .= (fst cursorP, snd cursorP - snd buttonBoxSize - 10)
  dataFromLastFrame <- M.lookup oId <$> use objectData
  let a = dataFromLastFrame ^? _Just . objectState . _MouseHeld
  objectData %= M.insertWith
    (\(Object a _ c) (Object _ b _) -> Object a b c)
    oId
    (Object (cursorP, buttonBoxSize) Inert SButton)
  addRectWithBorder cursor buttonBoxSizeT
    (if (isJust a)     then (0.0, 0.0, 1.0)
     else if isFocused then (0.0, 0.0, 0.5)
                       else (1.0, 0.0, 0.0)) (0.5, 1.0, 0.0)
  textPos <- toSPT ((fromIntegral $ fst cursorP) + 10, (fromIntegral $ snd cursorP) + 10)
  addText label textPos (2, 2)
  pure clicked

textInput :: ObjectId -> HamGui Bool
textInput oId = do
  clicked <- genericObjectInputCheck oId
  let buttonBoxSize = (400, 50)
  cursorP        <- use cursorPosition
  cursor         <- toSPT cursorP
  isFocused      <- fromMaybe False <$> ((fmap . fmap) ((==) oId) $ use focusedObject)
  object         <- (M.lookup oId <$> use objectData)
  let labelText   = fromMaybe "" $ object ^? _Just . privateState . _STextInput
  buttonBoxSizeT <- toSPTU buttonBoxSize
  cursorPosition .= (fst cursorP, snd cursorP - snd buttonBoxSize - 10)
  dataFromLastFrame <- M.lookup oId <$> use objectData
  let a = dataFromLastFrame ^? _Just . objectState . _MouseHeld
  addRectWithBorder cursor buttonBoxSizeT
    (if (isJust a)     then (0.0, 0.0, 1.0)
     else if isFocused then (0.0, 0.0, 0.5)
                       else (1.0, 0.0, 0.0)) (0.5, 1.0, 0.0)
  textPos <- toSPT ((fromIntegral $ fst cursorP) + 10, (fromIntegral $ snd cursorP) + 10)
  charsFromKeyboard <- fromMaybe "" <$> use (inputs . alphaNumPressed)
  let newLabel = if isFocused then labelText ++ charsFromKeyboard else labelText
  if isFocused
  then inputs . alphaNumPressed .= Nothing
  else pure ()
  objectData %= M.insertWith
    (\(Object a _ _) (Object _ b _) -> Object a b $ STextInput newLabel)
    oId
    (Object (cursorP, buttonBoxSize) Inert $ STextInput newLabel)
  addText newLabel textPos (2, 2)
  pure clicked