module Graphics.UI.HamGui.HamGui where

import qualified Data.Vector.Storable.Mutable as MV
import qualified Language.C.Inline            as C
import qualified Data.Map                     as M
import Control.Monad.State.Strict
import Foreign.C.Types
import Control.Lens
import Foreign.Ptr
import Data.Maybe
import Data.Type.Equality
import Type.Reflection

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

-- TODO: check this function/mechanism
uploadAlphaNums :: String -> HamGui ()
uploadAlphaNums a = do
  inputs . alphaNumPressed .= Just a

initHamGuiData :: MV.IOVector CFloat -> MV.IOVector CInt -> HamGuiData
initHamGuiData vMV eMV = HamGuiData vMV eMV 0 0 0 (SPP (-890) 0) M.empty (Input Nothing Nothing Nothing) (SPP 0 0) emptyFont Nothing Nothing M.empty []

clearBuffers :: HamGui ()
clearBuffers = do
  vI .= 0
  eI .= 0
  vertId         .= 0
  -- TODO: make window position start from top left corner
  cursorPosition .= (SPP 0 900)

processInputs :: HamGui () -- Is it really needed?
processInputs = do
  pure ()

newFrame :: HamGui ()
newFrame = do
  clearBuffers
  processInputs

windowStart :: WindowId -> String -> HamGui ()
windowStart wId wTitle = do
  w <- use $ windows . at wId
  Just (Window (SPP wx wy) (SPP sx sy) wt _) <- case w of
    Nothing  -> windows . at wId <.= Just (Window (SPP 0 0) (SPP 500 800) wTitle Inert)
    Just w@(Window wpos@(SPP wx wy) wsize@(SPP wsx wsy) wt wstatus) -> do
      input <- use $ inputs . mousePos
      lmb <- uses (inputs . mouseKeyState) $ (preview $ _Just . _1)
      new_w <- case (input, lmb) of
        (Just mouse@(SPP mx my), Just True) -> do
          case wstatus of
            MouseHeld (SPP px py) -> do
                let mdeltax = mx - px
                let mdeltay = my - py
                pure $ w & windowPos .~ SPP (wx + mdeltax) (wy + mdeltay) & windowStatus .~ MouseHeld mouse
            _                     -> do
              cursorPosition .= SPP wx (wy+wsy-50)
              (rect, rectsize, rectT@(SPT tx ty), rectsizeT@(SPT tsx tsy)) <- fitBoxOfSize (SPP wsx 100)
              if checkIfPointIsInsideBox mouse (rect, rectsize) 
                then pure $ w & windowStatus .~ MouseHeld mouse
                else pure $ w & windowStatus .~ Inert
              -- pure $ Window wpos wsize wt (MouseHeld $ SPP mx my)
        (_, _) -> pure $ w & windowStatus .~ Inert
      windows . at wId .= Just new_w
      pure $ Just new_w

  cursorPosition .= SPP wx wy
  (rect, rectsize, rectT@(SPT tx ty), rectsizeT@(SPT tsx tsy)) <- fitBoxOfSize (SPP sx sy)
  addRectWithBorder rectT rectsizeT (RGBC 0.1 0.1 0.1) (RGBC 0.9 0.9 0.9)
  addRectWithBorder (SPT tx (ty + tsy - 0.1)) (SPT tsx 0.1) (RGBC 0.2 0.2 0.2) (RGBC 0.9 0.9 0.9)
  addText wt (SPT tx (ty + tsy - 0.08)) (SPT 2 2) -- TODO: This is not SPT
  cursorPosition .= (SPP (wx + 30) (wy+700))
  pure ()

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
         -- Checking for character 2 times, can be fixed probably
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

fF = fromMaybe False

genericObjectInputCheck :: ObjectId -> HamGui GenericMouseInputResults
genericObjectInputCheck oId = do
  dataFromLastFrame <- use $ objectData . at oId
  mouseM            <- use $ inputs . mousePos
  lmbM              <- use $ inputs . mouseKeyState
  let heldStatus  = isJust $ dataFromLastFrame ^? _Just . objectState . _MouseHeld
  let clickStatus = fF $ lmbM ^? _Just . _1
  let hoverStatus = fF $ do
       obj   <- dataFromLastFrame
       mouse <- mouseM
       pure $ checkIfPointIsInsideBox mouse $ obj ^. boxBox
  heldObjectStatus <- use heldObject
  let otherObjectIsNotHeld = case heldObjectStatus of
        Nothing   -> True
        Just hoId -> hoId == oId
  case mouseM of
    Nothing    -> do
      updateObjState oId Inert
      pure ResultOnInert
    Just mouse -> do
      if clickStatus then
        if heldStatus then
          pure $ ResultOnClickHeld mouse
        else
          if hoverStatus then
            if otherObjectIsNotHeld then do
              heldObject    .= Just oId
              focusedObject .= Just oId
              updateObjState oId (MouseHeld mouse)
              pure $ ResultOnClickStart mouse
            else
              pure ResultOnInert
          else
            pure ResultOnInert
      else do
        when (heldObjectStatus == Just oId) $ heldObject .= Nothing
        if hoverStatus then do
          updateObjState oId (MouseHover mouse)
          if heldStatus then
            pure $ ResultOnClickEnd mouse
          else
            pure ResultOnInert -- Maybe introduce hover events
        else do
          updateObjState oId Inert
          pure ResultOnInert

-- TODO: Weird function, probably can be refactored to something better
fitBoxOfSize :: ScreenPositionProjected -> HamGui (ScreenPositionProjected, ScreenPositionProjected, ScreenPositionTotal, ScreenPositionTotal)
fitBoxOfSize box@(SPP w h) = do
  cursorP@(SPP cx cy) <- use cursorPosition
  cursor              <- toSPT cursorP
  boxSizeT            <- toSPTU (SPP w h)
  cursorPosition .= (SPP cx $ cy - h - 10)
  pure (cursorP, box, cursor, boxSizeT)

-- TODO: Generalize these functions
isObjFocused :: ObjectId -> HamGui Bool
isObjFocused oId = fF <$> ((fmap . fmap) ((==) oId) $ use focusedObject)

isObjHeld :: ObjectId -> HamGui Bool
isObjHeld oId = do
  dataFromLastFrame <- M.lookup oId <$> use objectData
  pure $ isJust $ dataFromLastFrame ^? _Just . objectState . _MouseHeld

updateObjData :: ObjectId -> (ScreenPositionProjected, ScreenPositionProjected) -> ObjectState -> HamGui ()
updateObjData oId box st = objectData %= M.insertWith (\(Object a _ _) (Object _ b _) -> Object a b st) oId (Object box Inert st)

updateObjState :: ObjectId -> UIState -> HamGui ()
updateObjState oId st = objectData . at oId . mapped . objectState .= st

-- TODO: Styles / style colors
getPrimaryColor isHeld isFocused = pure $
  (if isHeld         then (RGBC 0.0 0.0 1.0)
   else if isFocused then (RGBC 0.0 0.0 0.5)
                     else (RGBC 1.0 0.0 0.0))

getSecondaryColor _isHeld _isFocused = pure (RGBC 0.5 1.0 0.0)

-- TODO: Refactor this
fitTextLabel :: ScreenPositionProjected -> ScreenPositionProjected -> HamGui (ScreenPositionTotal)
fitTextLabel (SPP rx ry) _rectsize = toSPT (SPP ((fromIntegral rx) + 10) $ (fromIntegral ry) + 10)

button :: ObjectId -> String -> HamGui Bool
button oId label = do
  result                             <- genericObjectInputCheck oId
  (rect, rectsize, rectT, rectsizeT) <- fitBoxOfSize (SPP 250 50)
  isFocused                          <- isObjFocused oId
  isHeld                             <- isObjHeld oId
  primaryColor                       <- getPrimaryColor isHeld isFocused
  secondaryColor                     <- getSecondaryColor isHeld isFocused
  textPos                            <- fitTextLabel rect rectsize
  updateObjData oId (rect, rectsize) SButton
  addRectWithBorder rectT rectsizeT primaryColor secondaryColor
  addText label textPos (SPT 2 2) -- TODO: This is not SPT
  pure $ isJust $ result ^? _ResultOnClickEnd

checkbox :: ObjectId -> HamGui Bool
checkbox oId = do
  result                             <- genericObjectInputCheck oId
  (rect, rectsize, rectT, rectsizeT) <- fitBoxOfSize (SPP 50 50)
  isFocused                          <- isObjFocused oId
  isHeld                             <- isObjHeld oId
  primaryColor                       <- getPrimaryColor isHeld isFocused
  secondaryColor                     <- getSecondaryColor isHeld isFocused
  textPos                            <- fitTextLabel rect rectsize
  object                             <- M.lookup oId <$> use objectData
  let state                          = fF $ object ^? _Just . privateState . _SCheckBox
  let newstate = if isJust $ result ^? _ResultOnClickEnd then not state else state -- TODO: make state transitions more cute
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
  -- TODO:                              vvvvvvvvv make this look better
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

slider :: forall a. (Slidable a, Typeable a, Show a) => ObjectId -> a -> a -> a -> HamGui a
slider oId value val_min val_max = do
  result                             <- genericObjectInputCheck oId
  (rect@(SPP px py), rectsize@(SPP sx _), rectT@(SPT cornerx _), rectsizeT@(SPT sizex sizey)) 
                                     <- fitBoxOfSize (SPP 250 20)
  isFocused                          <- isObjFocused oId
  isHeld                             <- isObjHeld oId
  primaryColor                       <- getPrimaryColor isHeld isFocused
  secondaryColor                     <- getSecondaryColor isHeld isFocused
  objects                            <- use objectData
  let object = objects ^. at oId ^? _Just . privateState
  let clickedx = case result of
        ResultOnClickStart (SPP mx my) -> (Just mx)
        ResultOnClickHeld (SPP mx my) -> (Just mx)
        _ -> Nothing
  -- TODO: Yep
  (p,new_val) <- case clickedx of
        Just mx -> do
          let new_val = slideBetweenClamped px (px + sx) mx val_min val_max
          let percentage = fractionBetween val_min val_max new_val
          updateObjData oId (rect, rectsize) $ SSlider new_val
          pure $ (floor $ percentage * (fromIntegral sx) + (fromIntegral px), new_val)
        Nothing -> do
          let failsafe = do
                let percentage = fractionBetween val_min val_max value
                updateObjData oId (rect, rectsize) $ SSlider value
                pure $ (floor $ percentage * (fromIntegral sx) + (fromIntegral px), value)
          case object of
            Just v -> do
              case v of 
                SSlider v2 -> do
                  case testEquality (typeRep::TypeRep a) (typeOf' v2) of
                    Just Refl -> do
                      let percentage = fractionBetween val_min val_max v2
                      updateObjData oId (rect, rectsize) $ SSlider v2
                      pure $ (floor $ percentage * (fromIntegral sx) + (fromIntegral px), v2)
                    Nothing -> failsafe
                _ ->           failsafe
            Nothing ->         failsafe
  addRect rectT rectsizeT secondaryColor skipUV skipUV
  re <- toSPT (SPP (p-10) py)
  addRect re (SPT 0.1 sizey) primaryColor skipUV skipUV
  addText (show new_val) (SPT (cornerx+sizex+0.1) (re ^. _SPT . _2)) (SPT 2 2) -- TODO: This is not SPT
  pure new_val