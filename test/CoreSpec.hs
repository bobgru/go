module CoreSpec where

import           Test.Hspec
import           Test.QuickCheck
import           Game.Go.Core
import           Game.Go.Types

spec :: Spec
spec = do
    describe "Core" $ do 
      it "oppositePlayer is self-identity" $ quickCheck oppositePlayerIsSelfIdentity
      it "isOppositePlayer is accurate" $ quickCheck isOppositePlayerIsAccurate
      it "isSamePlayer is accurate" $ quickCheck isSamePlayerIsAccurate
      it "ownerOf is accurate" $ quickCheck ownerOfIsAccurate


oppositePlayerIsSelfIdentity :: Player -> Bool
oppositePlayerIsSelfIdentity p = oppositePlayer (oppositePlayer p) == p

isOppositePlayerIsAccurate :: Player -> Bool
isOppositePlayerIsAccurate p = isOppositePlayer p (oppositePlayer p)

isSamePlayerIsAccurate :: Player -> Bool
isSamePlayerIsAccurate p = isSamePlayer p p


ownerOfIsAccurate :: Dim -> Player -> Property
ownerOfIsAccurate d p  =  (d > 0) ==> ownerOf s' l == Just p
  where
    s  = initState d 0
    l  = (0, 0)
    s' = addMove p (Move l) s





instance Arbitrary Player where
  arbitrary = arbitraryBoundedEnum

