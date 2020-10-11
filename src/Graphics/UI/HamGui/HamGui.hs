{-# LANGUAGE RankNTypes, TypeFamilies #-}
module Graphics.UI.HamGui.HamGui where

import qualified Data.Vector.Storable.Mutable as MV
import qualified Language.C.Inline            as C
import qualified Data.Map                     as M
import Control.Monad.State.Strict
import Foreign.C.Types
import Control.Lens
import Foreign.Ptr
import Data.Maybe

import Graphics.UI.HamGui.BitMapFont
import Graphics.UI.HamGui.Types

C.context (C.baseCtx <> C.vecCtx)

toSPT :: ScreenPositionProjected -> HamGui ScreenPositionTotal
toSPT (SPP x y) = do
  (SPP sx sy) <- use screenSize
  pure $ SPT (fromIntegral x / fromIntegral sx * 2.0 - 1.0) (fromIntegral y / fromIntegral sy * 2.0 - 1.0)

toSPTU :: ScreenPositionProjected -> HamGui ScreenPositionTotal
toSPTU (SPP x y) = do
  (SPP sx sy) <- use screenSize
  pure $ SPT (fromIntegral x / fromIntegral sx * 2.0) (fromIntegral y / fromIntegral sy * 2.0)

skipUV :: UVCoordinate
skipUV = UVC (-1.0) (-1.0)

setScreenSize :: ScreenPositionProjected -> HamGui ()
setScreenSize = assign screenSize

uploadMouseState :: ScreenPositionProjected -> (Bool, Bool) -> HamGui ()
uploadMouseState a b = do
  inputs . mousePos .= Just a
  inputs . mouseKeyState .= Just b

uploadAlphaNums :: String -> HamGui ()
uploadAlphaNums a = do
  inputs . alphaNumPressed .= Just a

initHamGuiData :: u -> MV.IOVector CFloat -> MV.IOVector CInt -> HamGuiData u
initHamGuiData userData vMV eMV = HamGuiData vMV eMV 0 0 0 (SPP (-900) 0) M.empty (Input Nothing Nothing Nothing) (SPP 0 0) emptyFont Nothing userData

clearBuffers :: HamGui ()
clearBuffers = do
  vI .= 0
  eI .= 0
  vertId         .= 0
  -- TODO: make window position start from top left corner
  cursorPosition .= (SPP 0 900)

processInputs :: HamGui ()
processInputs = do
  pure ()

newFrame :: HamGui ()
newFrame = do
  clearBuffers
  processInputs

-- TODO: pass via Strict Storable struct
addRect :: ScreenPositionTotal -> ScreenPositionTotal -> RGBColor -> UVCoordinate -> UVCoordinate -> HamGui ()
addRect (SPT x0 y0) (SPT sx sy) (RGBC r g b) (UVC u0 v0) (UVC u1 v1) = do
  v <- use vertId
  vv <- use vertexDataL
  vi <- use vI
  ev <- use elemDataL
  ei <- use eI
  let vs = MV.length vv
  let es = MV.length ev
  when (vs < fromIntegral vi + 50) $ do
                                     newv <- liftIO $ MV.grow vv 1000
                                     vertexDataL .= newv
  when (es < fromIntegral ei + 50) $ do
                                     newe <- liftIO $ MV.grow ev 1000
                                     elemDataL .= newe
  addVertex (cx0   )  (cy0   )  (cu0) (cv1)
  addVertex (cx0+csx) (cy0   )  (cu1) (cv1)
  addVertex (cx0+csx) (cy0+csy) (cu1) (cv0)
  addVertex (cx0   )  (cy0+csy) (cu0) (cv0)
  addElem (v+0) (v+1) (v+2)
  addElem (v+0) (v+2) (v+3)
  vertId += 4
  pure ()
 where addVertex x y ux uy = do
         v <- use vertexDataL
         vId <- use vI
         newid <- liftIO $ [C.block|int {
           int vid = $(int vId);
           float *v = $vec-ptr:(float *v);
           v[vid++] = $(float x);  v[vid++] = $(float y);
           v[vid++] = $(float cr); v[vid++] = $(float cg); v[vid++] = $(float cb);
           v[vid++] = $(float ux); v[vid++] = $(float uy);
           return vid;
         }|]
         vI .= newid
       addElem i0 i1 i2 = do
         e <- use elemDataL
         eId <- use eI
         newid <- liftIO $ [C.block|int {
           int *e = $vec-ptr:(int *e);
           int eid = $(int eId);
           e[eid++] = $(int i0); e[eid++] = $(int i1); e[eid++] = $(int i2);
           return eid;
         }|]
         eI .= newid
       (cx0, cy0) = (CFloat x0, CFloat y0);(csx, csy) = (CFloat sx, CFloat sy); (cr, cg, cb) = (CFloat r, CFloat g, CFloat b); (cu0, cv0) = (CFloat u0, CFloat v0); (cu1, cv1) = (CFloat u1, CFloat v1)

addGlyph :: Char -> ScreenPositionTotal -> ScreenPositionTotal -> HamGui ()
addGlyph ch (SPT x y) (SPT w h) = do
  bmf <- use bitMapFont
  let f = bmf ^. charSet
  let char = M.lookup ch f
  case char of
    Nothing -> liftIO $ fail "pepega character not found"
    Just (CharDef cx cy sx sy _ox _oy _ax _ay) -> do
      addRect (SPT x y) (SPT w h) (RGBC 1.0 0.0 0.0) (UVC cx cy) (UVC (cx+sx) (cy+sy))

addText :: String -> ScreenPositionTotal -> ScreenPositionTotal -> HamGui ()
addText str (SPT x y) (SPT w h) = do
  bmf <- use bitMapFont
  go bmf str (x, y) (w, h)
 where go _ [] _ _ = pure ()
       go bmf (ch:rest) (x, y) (w, h) = do
         let f = bmf ^. charSet
         let char = M.lookup ch f
         case char of
           Nothing -> liftIO $ fail "pepega character not found"
           Just (CharDef _cx _cy sx sy ox oy ax ay) -> do
             addGlyph ch (SPT (x+ox*w) (y+oy*h)) (SPT (sx*w) (sy*h))
             go bmf rest (x+ax*w, y+ay*h) (w, h)
         pure ()

addRectWithBorder :: ScreenPositionTotal -> ScreenPositionTotal -> RGBColor -> RGBColor -> HamGui ()
addRectWithBorder p@(SPT x0 y0) s@(SPT sx sy) c@(RGBC _r _g _b) cb@(RGBC _rb _gb _bb) = do
  addRect p s cb skipUV skipUV
  addRect (SPT (x0+0.01) (y0+0.01)) (SPT (sx-0.02) (sy-0.02)) c skipUV skipUV

composeBuffers :: (Ptr CFloat -> IO ()) -> (Ptr CInt -> IO ()) -> HamGui ()
composeBuffers actionA actionE = do
  dv <- use vertexDataL
  ev <- use elemDataL
  liftIO $ MV.unsafeWith dv actionA
  liftIO $ MV.unsafeWith ev actionE
  pure ()

-- Actual widgetds

checkIfPointIsInsideBox :: ScreenPositionProjected -> (ScreenPositionProjected, ScreenPositionProjected) -> Bool
checkIfPointIsInsideBox (SPP x y) ((SPP bx by), (SPP sx sy)) = do
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

fitBoxOfSize :: ScreenPositionProjected -> HamGui (ScreenPositionProjected, ScreenPositionProjected, ScreenPositionTotal, ScreenPositionTotal)
fitBoxOfSize box@(SPP w h) = do
  cursorP@(SPP cx cy) <- use cursorPosition
  cursor              <- toSPT cursorP
  boxSizeT            <- toSPTU (SPP w h)
  cursorPosition .= (SPP cx $ cy - h - 10)
  pure (cursorP, box, cursor, boxSizeT)

isObjFocused :: ObjectId -> HamGui Bool
isObjFocused oId = fromMaybe False <$> ((fmap . fmap) ((==) oId) $ use focusedObject)

isObjHeld :: ObjectId -> HamGui Bool
isObjHeld oId = do
  dataFromLastFrame <- M.lookup oId <$> use objectData
  pure $ isJust $ dataFromLastFrame ^? _Just . objectState . _MouseHeld

updateObjData :: ObjectId -> (ScreenPositionProjected, ScreenPositionProjected) -> ObjectState -> HamGui ()
updateObjData oId box st = objectData %= M.insertWith (\(Object a _ _) (Object _ b _) -> Object a b st) oId (Object box Inert st)

getPrimaryColor isHeld isFocused = pure $
  (if isHeld         then (RGBC 0.0 0.0 1.0)
   else if isFocused then (RGBC 0.0 0.0 0.5)
                     else (RGBC 1.0 0.0 0.0))

getSecondaryColor _isHeld _isFocused = pure (RGBC 0.5 1.0 0.0)

fitTextLabel :: ScreenPositionProjected -> ScreenPositionProjected -> HamGui (ScreenPositionTotal)
fitTextLabel (SPP rx ry) _rectsize = toSPT (SPP ((fromIntegral rx) + 10) $ (fromIntegral ry) + 10)

button :: ObjectId -> String -> HamGui Bool
button oId label = do
  clicked                            <- genericObjectInputCheck oId
  (rect, rectsize, rectT, rectsizeT) <- fitBoxOfSize (SPP 250 50)
  isFocused                          <- isObjFocused oId
  isHeld                             <- isObjHeld oId
  primaryColor                       <- getPrimaryColor isHeld isFocused
  secondaryColor                     <- getSecondaryColor isHeld isFocused
  textPos                            <- fitTextLabel rect rectsize
  updateObjData     oId (rect, rectsize) SButton
  addRectWithBorder rectT rectsizeT primaryColor secondaryColor
  addText label textPos (SPT 2 2) -- TODO: This is not SPT
  pure clicked

checkbox :: ObjectId -> HamGui Bool
checkbox oId = do
  clicked                            <- genericObjectInputCheck oId
  (rect, rectsize, rectT, rectsizeT) <- fitBoxOfSize (SPP 50 50)
  isFocused                          <- isObjFocused oId
  isHeld                             <- isObjHeld oId
  primaryColor                       <- getPrimaryColor isHeld isFocused
  secondaryColor                     <- getSecondaryColor isHeld isFocused
  textPos                            <- fitTextLabel rect rectsize
  object                             <- M.lookup oId <$> use objectData
  let state                          = fromMaybe False $ object ^? _Just . privateState . _SCheckBox
  let newstate = if clicked then not state else state
  updateObjData     oId (rect, rectsize) (SCheckBox newstate)
  addRectWithBorder rectT rectsizeT primaryColor secondaryColor
  when state $ addText "X" textPos (SPT 2 2) -- TODO: This is not SPT
  pure newstate

textInput :: ObjectId -> HamGui String
textInput oId = do
  _                                  <- genericObjectInputCheck oId
  (rect, rectsize, rectT, rectsizeT) <- fitBoxOfSize (SPP 200 50)
  isFocused                          <- isObjFocused oId
  isHeld                             <- isObjHeld oId
  primaryColor                       <- getPrimaryColor isHeld isFocused
  secondaryColor                     <- getSecondaryColor isHeld isFocused
  textPos                            <- fitTextLabel rect rectsize
  object                             <- M.lookup oId <$> use objectData
  charsFromKeyboard                  <- fromMaybe "" <$> use (inputs . alphaNumPressed)
  let labelText                       = fromMaybe "" $ object ^? _Just . privateState . _STextInput
  let newLabel = if isFocused then labelText ++ charsFromKeyboard else labelText
  void $         if isFocused then inputs . alphaNumPressed .= Nothing else pure ()
  updateObjData     oId (rect, rectsize) $ STextInput newLabel
  addRectWithBorder rectT rectsizeT primaryColor secondaryColor
  addText newLabel textPos (SPT 2 2)
  pure newLabel

textLabel :: ObjectId -> String -> HamGui ()
textLabel oId label = do
  _                                  <- genericObjectInputCheck oId
  (rect, rectsize, _, _)             <- fitBoxOfSize (SPP 200 50)
  isFocused                          <- isObjFocused oId
  textPos                            <- fitTextLabel rect rectsize
  void $         if isFocused then inputs . alphaNumPressed .= Nothing else pure ()
  updateObjData     oId (rect, rectsize) $ STextLabel
  addText label textPos (SPT 2 2)
  pure ()

slider :: Slidable a => ObjectId -> a -> HamGui Bool
slider oId value = do
  clicked                            <- genericObjectInputCheck oId
  (rect, rectsize, rectT@(SPT cornerx cornery), rectsizeT@(SPT sizex sizey)) <- fitBoxOfSize (SPP 250 10)
  isFocused                          <- isObjFocused oId
  isHeld                             <- isObjHeld oId
  primaryColor                       <- getPrimaryColor isHeld isFocused
  secondaryColor                     <- getSecondaryColor isHeld isFocused
  -- textPos                            <- fitTextLabel rect rectsize
  updateObjData     oId (rect, rectsize) (SSlider value)
  addRect rectT rectsizeT secondaryColor skipUV skipUV
  addRect (SPT (cornerx + (sizex/2)) cornery) (SPT 0.1 0.1) primaryColor skipUV skipUV
  -- addRectWithBorder rectT rectsizeT primaryColor secondaryColor
  -- addText label textPos (SPT 2 2) -- TODO: This is not SPT
  pure clicked