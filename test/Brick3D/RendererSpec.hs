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

spec = do
  rendererSpec
  projectVertexSpec
  applyCameraTransformSpec

  
rendererSpec = do
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

almostEqual :: Float -> Float -> Bool
almostEqual a b = a+diff >= b && b <= a+diff 
  where
    diff = 0.1

almostEqualVertex :: Vertex -> Vertex -> Bool
almostEqualVertex (Vertex p1) (Vertex p2) = all (uncurry almostEqual) $ zip (fmap (p1^.) [_x,_y,_z]) (fmap (p1^.) [_x,_y,_z])



projectVertexSpec =
  describe "projectVertex" $ do
    context "when vertex is (0,0,z)" $
        prop "should be (0, 0) on screen coordinate no matter about focalLength" $ \f ->
            projectVertex f (Vertex $ V3 0 0 1) `shouldBe` (Vertex $ V3 0 0 0)

    context "when vertex is (x, y, 0)" $
        prop "should stay as is" $ \f x y ->
            projectVertex f (Vertex $ V3 x y 0) `shouldBe` (Vertex $ V3 x y 0)

    -- it "be 1 at" $ projectVertex f (Vertex $ V3 0 0 0)

applyCameraTransformSpec =
  describe "applyCameraTransform" $ do
    describe "position transformation" $ do
      context "when camera move to Right" $
        it "should move to Left" $ 
            applyCameraTransform (def&position._x.~5) (Point . Vertex $ V3 0 0 0)
                `shouldBe` (Point . Vertex $ V3 (-5) 0 0)
      context "when camera move to Up" $
        it "should move to Down" $
            applyCameraTransform (def&position._y.~5) (Point . Vertex $ V3 0 0 0)
                `shouldBe` (Point . Vertex $ V3 0 (-5) 0)
      context "when camera move foward" $
        it "should move backward" $ 
            applyCameraTransform (def&position._z.~5) (Point . Vertex $ V3 0 0 0)
                `shouldBe` (Point . Vertex $ V3 0 0 (-5))

    describe "rotation" $ do
      context "when camera rotate X+ axis" $
        it "should rotate X-" $
            let (Point result) = applyCameraTransform (rotateCamera def (V3 (pi/2) 0 0)) (Point . Vertex $ V3 1 1 1)
            in result `almostEqualVertex` (Vertex $ V3 1 1 (-1))
  
      context "when camera rotate Y+ axis" $
        it "should rotate Y-" $
            let (Point result) = applyCameraTransform (rotateCamera def (V3 0 (pi/2) 0)) (Point . Vertex $ V3 1 1 1)
            in result `almostEqualVertex` (Vertex $ V3 (-1) 1 1)

      context "when camera rotate Y+ axis" $
        it "should rotate Y-" $
            let (Point result) = applyCameraTransform (rotateCamera def (V3 0 (pi/2) 0)) (Point . Vertex $ V3 1 1 1)
            in result `almostEqualVertex` (Vertex $ V3 (-1) 1 1)
