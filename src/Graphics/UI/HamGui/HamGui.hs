module Graphics.UI.HamGui.HamGui where

import qualified Data.Vector.Storable.Mutable as MV
import qualified Data.Map                     as M
import Control.Monad.State.Lazy
import Foreign.C.Types
import Control.Lens
import Foreign.Ptr
import Foreign.Storable

import Graphics.UI.HamGui.BitMapFont
import Graphics.UI.HamGui.Types

logError :: String -> HamGui ()
logError = liftIO . putStrLn

skipUV :: UVCoordinate
skipUV = UVC (-1.0) (-1.0)

initHamGuiData :: MV.IOVector CFloat -> MV.IOVector CInt -> HamGuiData
initHamGuiData vMV eMV = 
  HamGuiData {
    _vertexDataL = vMV,
    _elemDataL = eMV,
    _vI = 0,
    _eI = 0,
    _vertId = 0,
    _screenSize = SS 0 0,
    _objectData = M.empty,
    _inputs = emptyInputs,
    _cursorPosition = SP 0 0,
    _bitMapFont = emptyFont,
    _focusedObject = Nothing,
    _heldObject = Nothing
  }

newFrame :: HamGui ()
newFrame = do
  vI .= 0
  eI .= 0
  vertId .= 0
  inputs .= emptyInputs

toSPN :: ScreenPosition -> HamGui ScreenPositionNormalized
toSPN (SP x y) = do
  SS sx sy <- use screenSize
  let fx = fromIntegral x / fromIntegral sx * 2.0 - 1.0
  let fy = 1.0 - fromIntegral y / fromIntegral sy * 2.0
  pure (SPN fx fy)

toSSN :: ScreenSize -> HamGui ScreenSizeNormalized
toSSN (SS x y) = do
  SS sx sy <- use screenSize
  let fx = realToFrac $ fromIntegral x / fromIntegral sx * 2.0
  let fy = realToFrac $ fromIntegral y / fromIntegral sy * 2.0
  pure (SSN fx fy)

data RectArg = RA ScreenRect | RAN ScreenRectNormalized
raToRan :: RectArg -> HamGui ScreenRectNormalized
raToRan rectArg = case rectArg of
    RA (SRect sloc ssize) -> do
      p <- toSPN sloc
      s <- toSSN ssize
      pure $ SRNect p s
    RAN rect -> pure rect

addRect :: RectArg -> RGBColor -> UVCoordinate -> UVCoordinate -> HamGui ()
addRect rectArg (RGBC r g b) (UVC u0 v0) (UVC u1 v1) = do
  SRNect (SPN x0 y0) (SSN sx sy) <- raToRan rectArg

  vv <- use vertexDataL
  vi <- use vI
  let vs = MV.length vv in 
      when (vs < fromIntegral vi + 7*4) $ do
        newv <- liftIO $ MV.grow vv vs
        vertexDataL .= newv
  ev <- use elemDataL
  ei <- use eI
  let es = MV.length ev in
      when (es < fromIntegral ei + 3*2) $ do
        newe <- liftIO $ MV.grow ev es
        elemDataL .= newe

  let (cx0, cy0)   = (CFloat x0, CFloat y0)
  let (csx, csy)   = (CFloat sx, CFloat sy)
  let (cr, cg, cb) = (CFloat r,  CFloat g, CFloat b)
  let (cu0, cv0)   = (CFloat u0, CFloat v0)
  let (cu1, cv1)   = (CFloat u1, CFloat v1)

  v <- use vertexDataL
  vId <- fromIntegral <$> (vI <<+= 7*4)
  liftIO $ MV.unsafeWith v $ (\ptr -> do
      pokeElemOff ptr (vId+0)  (cx0)     >> pokeElemOff ptr (vId+1)  (cy0-csy) >> pokeElemOff ptr (vId+2)  (cr) >> pokeElemOff ptr (vId+3)  (cg) >> pokeElemOff ptr (vId+4)  (cb) >> pokeElemOff ptr (vId+5)  (cu0) >> pokeElemOff ptr (vId+6)  (cv1)
      pokeElemOff ptr (vId+7)  (cx0+csx) >> pokeElemOff ptr (vId+8)  (cy0-csy) >> pokeElemOff ptr (vId+9)  (cr) >> pokeElemOff ptr (vId+10) (cg) >> pokeElemOff ptr (vId+11) (cb) >> pokeElemOff ptr (vId+12) (cu1) >> pokeElemOff ptr (vId+13) (cv1)
      pokeElemOff ptr (vId+14) (cx0+csx) >> pokeElemOff ptr (vId+15) (cy0)     >> pokeElemOff ptr (vId+16) (cr) >> pokeElemOff ptr (vId+17) (cg) >> pokeElemOff ptr (vId+18) (cb) >> pokeElemOff ptr (vId+19) (cu1) >> pokeElemOff ptr (vId+20) (cv0)
      pokeElemOff ptr (vId+21) (cx0)     >> pokeElemOff ptr (vId+22) (cy0)     >> pokeElemOff ptr (vId+23) (cr) >> pokeElemOff ptr (vId+24) (cg) >> pokeElemOff ptr (vId+25) (cb) >> pokeElemOff ptr (vId+26) (cu0) >> pokeElemOff ptr (vId+27) (cv0)
    )
  e    <- use elemDataL
  eId  <- fromIntegral <$> (eI <<+= 3*2)
  vidi <- fromIntegral <$> (vertId <<+= 4)
  liftIO $ MV.unsafeWith e $ (\ptr -> do
      pokeElemOff ptr (eId+0) (vidi) >> pokeElemOff ptr (eId+1) (vidi+1) >> pokeElemOff ptr (eId+2) (vidi+2)
      pokeElemOff ptr (eId+3) (vidi) >> pokeElemOff ptr (eId+4) (vidi+2) >> pokeElemOff ptr (eId+5) (vidi+3)
    )

addGlyph :: CharacterDefinition -> RectArg -> HamGui ()
addGlyph (CharDef cx cy sx sy _psx _psy _ox _oy _ax _ay) rect = do
  addRect rect (RGBC 1.0 0.0 0.0) (UVC cx cy) (UVC (cx+sx) (cy+sy))
  
offsetRectBy :: ScreenRectNormalized -> ScreenPositionNormalized -> ScreenRectNormalized
offsetRectBy (SRNect (SPN x y) size) (SPN ox oy) = (SRNect (SPN (x+ox) (y+oy)) size)

addText :: String -> RectArg -> HamGui ()
addText str rectArg = do
  SRNect (SPN gx gy) (SSN gsx gsy) <- raToRan rectArg
  bmf                              <- use $ bitMapFont . charSet
  bound                            <- use $ bitMapFont . boundingHeigh
  bound_offset                     <- fromIntegral <$> (use $ bitMapFont . boundingOffset)
  let pixel_side_x = gsx / fromIntegral bound
  let pixel_side_y = gsy / fromIntegral bound
  foldM_ (\offset char -> do
      let chardef = bmf ^. at char
      case chardef of
        Nothing -> (logError $ "Couldn't find character definition for { " ++ [char] ++ " } in current font") >> pure offset
        Just cd@(CharDef cx cy sx sy psx psy ox oy ax ay) -> do
          addGlyph cd 
            (RAN $ (SRNect 
              (SPN (gx + offset + pixel_side_x * ox                        ) 
                   (gy +          pixel_side_y * (psy + oy - bound_offset)))
              (SSN (realToFrac $ psx * pixel_side_x) 
                   (realToFrac $ psy * pixel_side_y))))
          pure $ offset + ax * pixel_side_x
    ) 0.0 str
  pure ()

composeBuffers :: (Ptr CFloat -> IO ()) -> (Ptr CInt -> IO ()) -> HamGui ()
composeBuffers actionA actionE = do
  dv <- use vertexDataL
  ev <- use elemDataL
  liftIO $ MV.unsafeWith dv actionA
  liftIO $ MV.unsafeWith ev actionE
  pure ()