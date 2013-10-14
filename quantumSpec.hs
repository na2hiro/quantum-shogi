import Test.Hspec
import Quantum 
import Data.Set as S(fromList, empty)

set = S.fromList

spec :: Spec
spec = do
    describe "simple" $ do
      it "Hi Ky*3, Hi*1" $
        checkMaxFromMoveUnpromoted [[[0,-2]],[[0,-2]],[[0,-2]],[[0,2]]] `shouldBe` Left (PieceExhausted (set[Ky,Hi]) 4)
      it "Hi Ky*2" $
        checkMaxFromMoveUnpromoted [[[0,-2]],[[0,-2]]] `shouldBe` return (set[])
      it "Hi Ky*2, Hi*1" $
        checkMaxFromMoveUnpromoted [[[0,-2]],[[0,-2]],[[0,2]]] `shouldBe` return (set[Ky,Hi])
      it "Hi Ky*1, Hi*2" $
        checkMaxFromMoveUnpromoted [[[0,-2]],[[0,2]],[[0,2]]] `shouldBe` Left (PieceExhausted (set[Hi]) 2)
      it "Hi*2" $
        checkMaxFromMoveUnpromoted [[[0,-2],[1,0]],[[0,2]]] `shouldBe` Left (PieceExhausted (set[Hi]) 2)
      it "(1,1)*4" $
        checkMaxFromMoveUnpromoted [[[1,1]],[[1,1]],[[1,1]],[[1,1]]] `shouldBe` return (set[Ka,Gi,Ou])
      it "(1,1)*5" $
        checkMaxFromMoveUnpromoted [[[1,1]],[[1,1]],[[1,1]],[[1,1]],[[1,1]]] `shouldBe` Left (PieceExhausted (set[Gi,Ka,Ou]) 5)
      it "(1,1)*4, (2,2)*1" $
        checkMaxFromMoveUnpromoted [[[1,1]],[[1,1]],[[1,1]],[[1,1]],[[2,2]]] `shouldBe` Left (PieceExhausted (set[Gi,Ka,Ou]) 5)
      it "(1,1)*3, (2,2)*1" $
        checkMaxFromMoveUnpromoted [[[1,1]],[[1,1]],[[1,1]],[[2,2]]] `shouldBe` return (set[Ka,Gi,Ou])
      it "(1,-2)(-1,-1)*1" $
        checkMaxFromNumsUnpromoted [([[0,-1]], 1), ([[1,-2],[-1,-1]],1)] `shouldBe` Left (InvalidMoveCombination 1)
      it "(1,-3)*1" $
        checkMaxFromMoveUnpromoted [[[1,1]], [[-1,-2]], [[1,-3]]] `shouldBe` Left (InvalidMove 2 0)
      it "(1,1)*2 (1,0)*2 (1,-1)*3" $
        checkMaxFromNumsUnpromoted [([[1,1]], 2), ([[1,0]], 2), ([[1,-1]], 3) ] `shouldBe` return (set [Gi,Ki,Ka,Hi,Ou])
    describe "ou" $do
      it "(1,0)(1,1)*1" $
        checkMaxFromNumsUnpromoted [([[1,0],[1,1]], 1)] `shouldBe` return (set [Ou])
      it "(1,0)(1,1)*2" $
        checkMaxFromNumsUnpromoted [([[1,0],[1,1]], 2)] `shouldBe` Left (PieceExhausted (set [Ou]) 2)
      it "(1,0)(1,1)*1, (1,1)*2" $
        checkMaxFromNumsUnpromoted [([[1,0],[1,1]], 1), ([[1,1]], 2)] `shouldBe` return (set[Ou])
      it "(1,0)(1,1)*1, (1,1)*3" $
        checkMaxFromNumsUnpromoted [([[1,0],[1,1]], 1), ([[1,1]], 3)] `shouldBe` return (set[Ka,Gi,Ou])
      it "(1,0)(1,1)*1, (1,1)*4" $
        checkMaxFromNumsUnpromoted [([[1,0],[1,1]], 1), ([[1,1]], 4)] `shouldBe` Left (PieceExhausted (set[Gi,Ka,Ou]) 5)
      it "(1,0)(1,1)*1, (0,1)*3" $
        checkMaxFromNumsUnpromoted [([[1,0],[1,1]], 1), ([[0,1]], 3)] `shouldBe` return (set[Ki,Hi,Ou])
      it "(1,0)(1,1)*1, (0,1)*4" $
        checkMaxFromNumsUnpromoted [([[1,0],[1,1]], 1), ([[0,1]], 4)] `shouldBe` Left (PieceExhausted (set[Ki,Hi,Ou]) 5)
    describe "simple detailed" $ do
      it "(0,-2)*2" $
        checkMaxDetailedFromMoveUnpromoted [[[0,-2]],[[0,-2]]] `shouldBe` return [(set[],[0,1])]
      it "(0,-2)*2, (0,2)*1" $
        checkMaxDetailedFromMoveUnpromoted [[[0,-2]],[[0,-2]],[[0,2]]] `shouldBe` return [(set[Hi],[2]),(set[],[0,1]),(set[Ky,Hi],[0,1,2])]

    describe "ou detailed"$ do
      it "(1,0)(1,1)*1, (1,1)*2" $
        checkMaxDetailedFromNumsUnpromoted [([[1,0],[1,1]], 1), ([[1,1]], 2)] `shouldBe` return [(fromList [Ou],[0]),(fromList [],[1,2]),(fromList [],[1,2,0])]
      it "(1,0)(1,1)*1, (1,1)*3" $
        checkMaxDetailedFromNumsUnpromoted [([[1,0],[1,1]], 1), ([[1,1]], 3)] `shouldBe` return [(fromList [Ou],[0]),(fromList [],[1,2,3]),(fromList [Gi,Ka,Ou],[1,2,3,0])]

    describe "simple check" $ do
      it "(0,-2)*2" $
        checkUnpromoted [[[0,-2]],[[0,-2]]] `shouldBe` return ([set[Ky,Hi],set[Ky,Hi]], set[])
      it "(0,-2)*2, (0,2)*1" $
        checkUnpromoted [[[0,-2]],[[0,-2]],[[0,2]]] `shouldBe` return ([set[Ky],set[Ky],set[Hi]], set[Ky,Hi])

    describe "ou check"$ do
      it "(1,0)(1,1)*1, (1,1)*2" $
        checkFromNumsUnpromoted [([[1,0],[1,1]], 1), ([[1,1]], 2)] `shouldBe` return ([set[Ou],set[Gi,Ka],set[Gi,Ka]], set[Ou])
      it "(1,0)(1,1)*1, (1,1)*3" $
        checkFromNumsUnpromoted [([[1,0],[1,1]], 1), ([[1,1]], 3)] `shouldBe` return ([set[Ou],set[Gi,Ka],set[Gi,Ka],set[Gi,Ka]], set[Ou,Gi,Ka])

    describe "simple promote"$ do
      it "(0,-2)nari*1" $
        check [[([0,-2], True)]] `shouldBe` return ([set[Hi]], set[Hi])
      it "(1,1)nari(1,1)*1" $
        check [[([1,1], True),([1,1],False)]] `shouldBe` return ([set[Ka]], set[Ka])
