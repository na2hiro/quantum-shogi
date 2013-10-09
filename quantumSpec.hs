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
      it "(1,0)(1,1)*1, (1,1)*2" $
        checkMaxFromNums [([[1,0],[1,1]], 1), ([[1,1]], 2)] `shouldBe` return (set[Ou])
      it "(1,0)(1,1)*1, (1,1)*3" $
        checkMaxFromNums [([[1,0],[1,1]], 1), ([[1,1]], 3)] `shouldBe` return (set[Ka,Gi,Ou])
      it "(1,0)(1,1)*1, (1,1)*4" $
        checkMaxFromNums [([[1,0],[1,1]], 1), ([[1,1]], 4)] `shouldBe` Left (PieceExhausted (set[Gi,Ka,Ou]) 5)
      it "(1,0)(1,1)*1, (0,1)*3" $
        checkMaxFromNums [([[1,0],[1,1]], 1), ([[0,1]], 3)] `shouldBe` return (set[Ki,Hi,Ou])
      it "(1,0)(1,1)*1, (0,1)*4" $
        checkMaxFromNums [([[1,0],[1,1]], 1), ([[0,1]], 4)] `shouldBe` Left (PieceExhausted (set[Ki,Hi,Ou]) 5)
    describe "simple detailed" $ do
      it "(0,-2)*2" $
        checkMaxDetailedFromMove [[[0,-2]],[[0,-2]]] `shouldBe` return [(set[],[0,1])]
      it "(0,-2)*2, (0,2)*1" $
        checkMaxDetailedFromMove [[[0,-2]],[[0,-2]],[[0,2]]] `shouldBe` return [(set[Hi],[2]),(set[],[0,1]),(set[Ky,Hi],[0,1,2])]

    describe "ou detailed"$ do
      it "(1,0)(1,1)*1, (1,1)*2" $
        checkMaxDetailedFromNums [([[1,0],[1,1]], 1), ([[1,1]], 2)] `shouldBe` return [(fromList [Ou],[0]),(fromList [],[1,2]),(fromList [],[1,2,0])]
      it "(1,0)(1,1)*1, (1,1)*3" $
        checkMaxDetailedFromNums [([[1,0],[1,1]], 1), ([[1,1]], 3)] `shouldBe` return [(fromList [Ou],[0]),(fromList [],[1,2,3]),(fromList [Gi,Ka,Ou],[1,2,3,0])]

    describe "simple check" $ do
      it "(0,-2)*2" $
        check [[[0,-2]],[[0,-2]]] `shouldBe` return ([set[Ky,Hi],set[Ky,Hi]], set[])
      it "(0,-2)*2, (0,2)*1" $
        check [[[0,-2]],[[0,-2]],[[0,2]]] `shouldBe` return ([set[Ky],set[Ky],set[Hi]], set[Ky,Hi])

    describe "ou check"$ do
      it "(1,0)(1,1)*1, (1,1)*2" $
        checkFromNums [([[1,0],[1,1]], 1), ([[1,1]], 2)] `shouldBe` return ([set[Ou],set[Gi,Ka],set[Gi,Ka]], set[Ou])
      it "(1,0)(1,1)*1, (1,1)*3" $
        checkFromNums [([[1,0],[1,1]], 1), ([[1,1]], 3)] `shouldBe` return ([set[Ou],set[Gi,Ka],set[Gi,Ka],set[Gi,Ka]], set[Ou,Gi,Ka])
