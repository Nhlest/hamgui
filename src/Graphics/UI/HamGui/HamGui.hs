module Graphics.UI.HamGui.HamGui where

import Data.Vector.Storable hiding ((++), forM_)
import Control.Applicative (liftA)
import Control.Monad.State.Lazy
import qualified Data.Map as M
import Control.Monad (ap)
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

updateMouseState :: ScreenPositionProjected -> (Bool, Bool) -> HamGui ()
updateMouseState a b = do
  inputs . mousePos .= Just a
  inputs . mouseKeyState .= Just b

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

addGlyph :: BitMapFont -> Char -> ScreenPositionTotal -> (Float, Float) -> HamGui ()
addGlyph bmf ch (x, y) (w, h) = do
  let f = bmf ^. charSet 
  let char = M.lookup ch f
  case char of
    Nothing -> liftIO $ fail "pepega character not found"
    Just (CharDef cx cy sx sy ox oy ax ay) -> do
      addRect (x, y) (w, h) (1.0, 0.0, 0.0) (cx, cy) ((cx+sx), (cy+sy))

addText :: BitMapFont -> String -> ScreenPositionTotal -> (Float, Float) -> HamGui ()
addText bmf str (x, y) (w, h) = do
  go str (x, y) (w, h)
 where go [] _ _ = pure ()
       go (ch:rest) (x, y) (w, h) = do
         let f = bmf ^. charSet 
         let char = M.lookup ch f
         case char of
           Nothing -> liftIO $ fail "pepega character not found"
           Just (CharDef cx cy sx sy ox oy ax ay) -> do
             addGlyph bmf ch (x+ox*w, y+oy*h) (sx*w, sy*h)
             go rest (x+ax*w, y+ay*h) (w, h)
         pure ()
   
addRectWithBorder :: ScreenPositionTotal -> ScreenPositionTotal -> (Float, Float, Float) -> (Float, Float, Float) -> HamGui ()
addRectWithBorder p@(x0, y0) s@(sx, sy) c@(r, g, b) cb@(rb, gb, bb) = do
  addRect p s cb skipUV skipUV
  addRect (x0+0.01, y0+0.01) (sx-0.02, sy-0.02) c skipUV skipUV

initHamGuiData :: HamGuiData
initHamGuiData = HamGuiData [] [] 0 (0, 0) M.empty (Input Nothing Nothing) (0, 0)

clearBuffers :: HamGui ()
clearBuffers = do
  vertexDataL .= []
  elemDataL   .= []
  vertId      .= 0
  cursorPosition .= (512, 512)

composeBuffers :: (Ptr CFloat -> IO ()) -> (Ptr CInt -> IO ()) -> HamGui ()
composeBuffers actionA actionE = do
  dv <- fromList <$> use vertexDataL
  ev <- fromList <$> use elemDataL
  liftIO $ unsafeWith dv actionA
  liftIO $ unsafeWith ev actionE
  pure ()

-- Actual widgetds

checkIfPointIsInsideBox :: ScreenPositionProjected -> (ScreenPositionProjected, ScreenPositionProjected) -> Bool
checkIfPointIsInsideBox (x, y) ((bx, by), (sx, sy)) = do
  x >= bx && x <= bx + sx && y >= by && y <= by + sy

button :: BitMapFont -> ObjectId -> String -> HamGui Bool -- TODO: sort out this fucking crap
button bmf oId label = do
  let buttonBoxSize = (200, 50)
  buttonBoxSizeT <- toSPTU buttonBoxSize
  cursor <- use cursorPosition
  cursorT <- toSPT cursor
  cursorPosition .= (fst cursor, snd cursor - snd buttonBoxSize - 10)
  boxFromLastFrame <- M.lookup oId <$> use objectData
  res <- case boxFromLastFrame of
          Nothing -> pure False
          Just obj -> do
            let box = obj ^. boxBox
            mouse <- use $ inputs . mousePos
            case mouse of 
              Nothing -> pure False
              Just m -> pure $ checkIfPointIsInsideBox m box 
  o <- use objectData
  objectData .= M.insert oId (Object (cursor, buttonBoxSize)) o
  addRectWithBorder cursorT buttonBoxSizeT (if res then (0.0, 0.0, 1.0) else (1.0, 0.0, 0.0)) (0.5, 1.0, 0.0)
  tt <- toSPT ((fromIntegral $ fst cursor) + 10, (fromIntegral $ snd cursor) + 10)
  addText bmf label tt (2, 2)
  lmb <- fst <$> fromJust <$> (use $ inputs . mouseKeyState) -- TODO prism shite
  pure (res && lmb)