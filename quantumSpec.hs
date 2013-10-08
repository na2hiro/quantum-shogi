import Test.Hspec
import Quantum 

spec :: Spec
spec = do
    describe "hoge" $ do
      it "Hi Ky*3, Hi*1" $
        checkMaxFromMove [[[0,-2]],[[0,-2]],[[0,-2]],[[0,2]]] `shouldBe` False
      it "Hi Ky*2, Hi*1" $
        checkMaxFromMove [[[0,-2]],[[0,-2]],[[0,2]]] `shouldBe` True
      it "Hi Ky*1, Hi*2" $
        checkMaxFromMove [[[0,-2]],[[0,2]],[[0,2]]] `shouldBe` False
      it "Hi*2" $
        checkMaxFromMove [[[0,-2],[1,0]],[[0,2]]] `shouldBe` False
      it "(1,1)*4" $
        checkMaxFromMove [[[1,1]],[[1,1]],[[1,1]],[[1,1]]] `shouldBe` True
      it "(1,1)*5" $
        checkMaxFromMove [[[1,1]],[[1,1]],[[1,1]],[[1,1]],[[1,1]]] `shouldBe` False
      it "(1,1)*5, (2,2)*1" $
        checkMaxFromMove [[[1,1]],[[1,1]],[[1,1]],[[1,1]],[[2,2]]] `shouldBe` False
      it "(1,1)*4, (2,2)*1" $
        checkMaxFromMove [[[1,1]],[[1,1]],[[1,1]],[[2,2]]] `shouldBe` True
      it "(1,-2)(-1,-1)*1" $
        checkMaxFromNums [([[1,-2],[-1,-1]],1)] `shouldBe` False
