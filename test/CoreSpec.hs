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
      it "ownerOf is accurate when true" $ quickCheck ownerOfIsAccurateWhenTrue
      it "ownerOf is accurate when false" $ quickCheck ownerOfIsAccurateWhenFalse


oppositePlayerIsSelfIdentity :: Player -> Bool
oppositePlayerIsSelfIdentity p = oppositePlayer (oppositePlayer p) == p

isOppositePlayerIsAccurate :: Player -> Bool
isOppositePlayerIsAccurate p = isOppositePlayer p (oppositePlayer p)

isSamePlayerIsAccurate :: Player -> Bool
isSamePlayerIsAccurate p = isSamePlayer p p


-- TODO randomize location checked
ownerOfIsAccurateWhenTrue :: Dim -> Player -> Property
ownerOfIsAccurateWhenTrue d p  =  (d > 0) ==> ownerOf s' l == Just p
  where
    s  = initState d 0
    l  = (0, 0)
    s' = addMove p (Move l) s

-- TODO randomize location checked
ownerOfIsAccurateWhenFalse :: Dim -> Player -> Property
ownerOfIsAccurateWhenFalse d p  =  (d > 0) ==> ownerOf s l == Nothing
  where
    s  = initState d 0
    l  = (0, 0)





instance Arbitrary Player where
  arbitrary = arbitraryBoundedEnum

