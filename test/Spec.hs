import Test.Hspec

import qualified Data.Map as M
import Graphics.UI.HamGui.BDF
import Graphics.UI.HamGui.BitMapFont

main :: IO ()
main = do
  hspec $ do
    describe "BDF Testing" $ do
      it "fillTillSide" $ do
        fillTillSide [[1,1,1,1], [1,1,1,1], [1,1,1,1]] 6 2 `shouldBe` [[1,1,1,1], [1,1,1,1,0,0], [1,1,1,1,0,0]]
      it "tryToFit simple 2x2 character" $ do
        tryToFit 8 [Character 1 (3,3) (1,0) (3, 3, 0, 0) [3,3]]
          `shouldBe`
          Just ([[255,255,0,0,0,0,0,0],
                 [255,255,0,0,0,0,0,0]], M.fromList [('\SOH', CharDef 0.0 0.125 0.25 0.25 0 0 0.125 0)])

      -- it "returns the first element of an *arbitrary* list" $
      --   property $ \x xs -> head (x:xs) == (x :: Int)

      -- it "throws an exception if used with an empty list" $ do
      --   evaluate (head []) `shouldThrow` anyException
