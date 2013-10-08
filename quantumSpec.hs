import Test.Hspec
import Quantum 
import Data.Set as S(fromList)

spec :: Spec
spec = do
    describe "hoge" $ do
      it "Hi Ky*3, Hi*1" $
        checkMaxFromMove [[[0,-2]],[[0,-2]],[[0,-2]],[[0,2]]] `shouldBe` Left (PieceExhausted (S.fromList[Ky,Hi]) 4)
      it "Hi Ky*2, Hi*1" $
        checkMaxFromMove [[[0,-2]],[[0,-2]],[[0,2]]] `shouldBe` return True
      it "Hi Ky*1, Hi*2" $
        checkMaxFromMove [[[0,-2]],[[0,2]],[[0,2]]] `shouldBe` return False
      it "Hi*2" $
        checkMaxFromMove [[[0,-2],[1,0]],[[0,2]]] `shouldBe` return False
      it "(1,1)*4" $
        checkMaxFromMove [[[1,1]],[[1,1]],[[1,1]],[[1,1]]] `shouldBe` return True
      it "(1,1)*5" $
        checkMaxFromMove [[[1,1]],[[1,1]],[[1,1]],[[1,1]],[[1,1]]] `shouldBe` return False
      it "(1,1)*5, (2,2)*1" $
        checkMaxFromMove [[[1,1]],[[1,1]],[[1,1]],[[1,1]],[[2,2]]] `shouldBe` return False
      it "(1,1)*4, (2,2)*1" $
        checkMaxFromMove [[[1,1]],[[1,1]],[[1,1]],[[2,2]]] `shouldBe` return True
      it "(1,-2)(-1,-1)*1" $
        checkMaxFromNums [([[0,-1]], 1), ([[1,-2],[-1,-1]],1)] `shouldBe` Left (InvalidMoveCombination 1)
      it "(1,-3)*1" $
        checkMaxFromMove [[[1,1]], [[-1,-2]], [[1,-3]]] `shouldBe` Left (InvalidMove 2 0)
