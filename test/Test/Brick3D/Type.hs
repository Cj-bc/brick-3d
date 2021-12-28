{-# LANGUAGE FlexibleInstances #-}
module Test.Brick3D.Type where
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Brick3D.Type
import Linear.V3 (V3(..), _x, _y, _z)
import Data.Functor.Compose
import Lens.Micro.Platform ((^.))

instance Arbitrary Vertex where
  arbitrary = Vertex <$> (V3 <$> arbitrary <*> arbitrary <*> arbitrary)

instance CoArbitrary Vertex where
  coarbitrary v = coarbitrary (v^.v_normal._x)
                . coarbitrary (v^.v_normal._y)
                . coarbitrary (v^.v_normal._z)

instance Arbitrary Primitive where
  arbitrary = Triangle <$> arbitrary <*> arbitrary <*> arbitrary

instance Show (Vertex -> [Vertex]) where
  show _ = "CUSTOM function"

verticesTest = 
  describe "vertices" $ do
    -- https://hackage.haskell.org/package/lens-5.0.1/docs/Control-Lens-Traversal.html#t:Traversal
    prop "should satisfy 1st law" $ \prim ->
      (vertices pure prim :: [Primitive]) == (pure prim :: [Primitive])
    prop "should satisfy 2nd law" $ \prim ->
      let f = pure :: Vertex -> [Vertex]
          g = const [] :: Vertex -> [Vertex]
      in
        (fmap (vertices f) . vertices g) prim
        == ((getCompose . vertices (Compose . fmap f . g)) prim)
