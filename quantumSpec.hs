import Test.Hspec
import Quantum 
import Data.Set as S(fromList, empty)

set = S.fromList

spec :: Spec
spec = do
    describe "simple" $ do
      it "Hi Ky*3, Hi*1" $
        checkMaxFromMove [[[0,-2]],[[0,-2]],[[0,-2]],[[0,2]]] `shouldBe` Left (PieceExhausted (set[Ky,Hi]) 4)
      it "Hi Ky*2" $
        checkMaxFromMove [[[0,-2]],[[0,-2]]] `shouldBe` return (set[])
      it "Hi Ky*2, Hi*1" $
        checkMaxFromMove [[[0,-2]],[[0,-2]],[[0,2]]] `shouldBe` return (set[Ky,Hi])
      it "Hi Ky*1, Hi*2" $
        checkMaxFromMove [[[0,-2]],[[0,2]],[[0,2]]] `shouldBe` Left (PieceExhausted (set[Hi]) 2)
      it "Hi*2" $
        checkMaxFromMove [[[0,-2],[1,0]],[[0,2]]] `shouldBe` Left (PieceExhausted (set[Hi]) 2)
      it "(1,1)*4" $
        checkMaxFromMove [[[1,1]],[[1,1]],[[1,1]],[[1,1]]] `shouldBe` return (set[Ka,Gi,Ou])
      it "(1,1)*5" $
        checkMaxFromMove [[[1,1]],[[1,1]],[[1,1]],[[1,1]],[[1,1]]] `shouldBe` Left (PieceExhausted (set[Gi,Ka,Ou]) 5)
      it "(1,1)*4, (2,2)*1" $
        checkMaxFromMove [[[1,1]],[[1,1]],[[1,1]],[[1,1]],[[2,2]]] `shouldBe` Left (PieceExhausted (set[Gi,Ka,Ou]) 5)
      it "(1,1)*3, (2,2)*1" $
        checkMaxFromMove [[[1,1]],[[1,1]],[[1,1]],[[2,2]]] `shouldBe` return (set[Ka,Gi,Ou])
      it "(1,-2)(-1,-1)*1" $
        checkMaxFromNums [([[0,-1]], 1), ([[1,-2],[-1,-1]],1)] `shouldBe` Left (InvalidMoveCombination 1)
      it "(1,-3)*1" $
        checkMaxFromMove [[[1,1]], [[-1,-2]], [[1,-3]]] `shouldBe` Left (InvalidMove 2 0)
      it "(1,1)*2 (1,0)*2 (1,-1)*3" $
        checkMaxFromNums [([[1,1]], 2), ([[1,0]], 2), ([[1,-1]], 3) ] `shouldBe` return (set [Gi,Ki,Ka,Hi,Ou])
    describe "ou" $do
      it "(1,0)(1,1)*1" $
        checkMaxFromNums [([[1,0],[1,1]], 1)] `shouldBe` return (set [Ou])
      it "(1,0)(1,1)*2" $
        checkMaxFromNums [([[1,0],[1,1]], 2)] `shouldBe` Left (PieceExhausted (set [Ou]) 2)
      it "(1,0)(1,1)*1, (1,1)*3" $
        checkMaxFromNums [([[1,0],[1,1]], 1), ([[1,1]], 3)] `shouldBe` return (set[Ka,Gi,Ou])
      it "(1,0)(1,1)*1, (1,1)*4" $
        checkMaxFromNums [([[1,0],[1,1]], 1), ([[1,1]], 4)] `shouldBe` Left (PieceExhausted (set[Gi,Ka,Ou]) 5)
      it "(1,0)(1,1)*1, (0,1)*3" $
        checkMaxFromNums [([[1,0],[1,1]], 1), ([[0,1]], 3)] `shouldBe` return (set[Ki,Hi,Ou])
      it "(1,0)(1,1)*1, (0,1)*4" $
        checkMaxFromNums [([[1,0],[1,1]], 1), ([[0,1]], 4)] `shouldBe` Left (PieceExhausted (set[Ki,Hi,Ou]) 5)
