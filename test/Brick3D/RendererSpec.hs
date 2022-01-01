module Brick3D.RendererSpec where

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Lens.Micro.Platform
import Linear.V3 (V3(..), _x, _y, _z)
import Data.Default (def)
import Brick3D.Renderer
import Brick3D.Type
import Brick3D.Camera

rendererTest = do
  describe "farNearClip" $ do
    let cam = Camera (V3 0.0 0.0 0.0) (V3 0.0 0.0 0.0) 60 1.0 2.0
    
    context "when Primitive is behind far clip plane" $
      it "should not be visible" $ forAll
      (arbitrary `suchThat` (\z -> z < (cam^.position._z)-(cam^.farClip)))
      $ \z -> farNearClip cam (Point (Vertex (V3 0.0 0.0 z))) `shouldBe` False

    context "when Primitive is in front of near clip plane" $
      it "should not be visible" $ forAll
      (arbitrary `suchThat` (\z -> (cam^.position._z)-(cam^.nearClip) < z))
      $ \z -> farNearClip cam (Point (Vertex $ V3 0 0 z)) `shouldBe` False

    context "when Primitive is between far clip plane and near clip plane" $
      it "should be visible" $ forAll
      (arbitrary `suchThat` (\z -> (cam^.position._z)-(cam^.farClip) <= z
                                   && z <= (cam^.position._z)-(cam^.nearClip)))
      $ \z -> farNearClip cam (Point (Vertex $ V3 0 0 z)) `shouldBe` True
