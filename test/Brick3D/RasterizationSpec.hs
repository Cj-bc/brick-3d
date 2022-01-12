module Brick3D.RasterizationSpec where

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Linear.V2 (V2(..))
import Brick3D.Rasterization


spec = do
  isInsideOfTriSpec



isInsideOfTriSpec = do
  describe "isInsideOfTri" $ do
        -- prepareTestVertices :: Testable prop => Float -> prop
    context "when the vertex is inside" $ do
      prop "should be True" $
        forAll (elements $ (/100) <$> [0..100]) $ \x ->
                                  let maxY = (-x + 1)*100
                                      ys = (/100) <$> [0..maxY]
                                  in forAll (elements ys) $ \y ->
              (V2 x y) `isInsideOfTri` (V2 0 0, V2 0 1, V2 1 0) `shouldBe` True
    context "when the vertex is outside" $ do
      context "if x is out of it" $
        prop "should be False" $
          forAll (arbitrary `suchThat` (\a -> a < 0 || 1 < a)) $ \x y ->
                     (V2 x y) `isInsideOfTri` (V2 0 0, V2 0 1, V2 1 0) `shouldBe` False
      context "if " $ prop "should be False" $
        forAll (arbitrary `suchThat` (\a -> a < 0 || 1 < a)) $ \x ->
                                  let maxY = (-x + 1)
                                  in forAll (arbitrary `suchThat` (\a -> a < 0 || maxY < a)) $ \y ->
                                    (V2 x y) `isInsideOfTri` (V2 0 0, V2 0 1, V2 1 0) `shouldBe` False
