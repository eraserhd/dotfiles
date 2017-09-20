import Test.Hspec (hspec, describe, it, shouldBe)

import Daily (everything)

main :: IO ()
main = hspec $ do
  describe "daily stuff" $ do
    it "should not change boolean logic" $ do
      True `shouldBe` True
