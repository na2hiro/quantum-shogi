import Test.Hspec
import Quantum 
import Data.Set as S(fromList, empty)
import Control.Monad(liftM)

set = S.fromList

spec :: Spec
spec = do
    describe "simple" $ do
      it "Hi Ky*3, Hi*1" $
        getLimitedUnpromoted [[[0,-2]],[[0,-2]],[[0,-2]],[[0,2]]] `shouldBe` Left (PieceExhausted (set[Ky,Hi]) 4)
      it "Hi Ky*2" $
        getLimitedUnpromoted [[[0,-2]],[[0,-2]]] `shouldBe` return (set[])
      it "Hi Ky*2, Hi*1" $
        getLimitedUnpromoted [[[0,-2]],[[0,-2]],[[0,2]]] `shouldBe` return (set[Ky,Hi])
      it "Hi Ky*1, Hi*2" $
        getLimitedUnpromoted [[[0,-2]],[[0,2]],[[0,2]]] `shouldBe` Left (PieceExhausted (set[Hi]) 2)
      it "Hi*2" $
        getLimitedUnpromoted [[[0,-2],[1,0]],[[0,2]]] `shouldBe` Left (PieceExhausted (set[Hi]) 2)
      it "(1,1)*4" $
        getLimitedUnpromoted [[[1,1]],[[1,1]],[[1,1]],[[1,1]]] `shouldBe` return (set[Ka,Gi,Ou])
      it "(1,1)*5" $
        getLimitedUnpromoted [[[1,1]],[[1,1]],[[1,1]],[[1,1]],[[1,1]]] `shouldBe` Left (PieceExhausted (set[Gi,Ka,Ou]) 5)
      it "(1,1)*4, (2,2)*1" $
        getLimitedUnpromoted [[[1,1]],[[1,1]],[[1,1]],[[1,1]],[[2,2]]] `shouldBe` Left (PieceExhausted (set[Gi,Ka,Ou]) 5)
      it "(1,1)*3, (2,2)*1" $
        getLimitedUnpromoted [[[1,1]],[[1,1]],[[1,1]],[[2,2]]] `shouldBe` return (set[Ka,Gi,Ou])
      it "(1,-2)(-1,-1)*1" $
        getLimitedUnpromoted ([[0,-1]]:[[1,-2],[-1,-1]]:[]) `shouldBe` Left (InvalidMoveCombination 1)
      it "(1,-3)*1" $
        getLimitedUnpromoted [[[1,1]], [[-1,-2]], [[1,-3]]] `shouldBe` Left (InvalidMove 2 0)
      it "(1,1)*2 (1,0)*2 (1,-1)*3" $
        getLimitedUnpromoted (replicate 2 [[1,1]] ++ replicate 2 [[1,0]] ++ replicate 3 [[1,-1]]) `shouldBe` return (set [Gi,Ki,Ka,Hi,Ou])
    describe "ou" $do
      it "(1,0)(1,1)*1" $
        getLimitedUnpromoted [[[1,0],[1,1]]] `shouldBe` return (set [Ou])
      it "(1,0)(1,1)*2" $
        getLimitedUnpromoted (replicate 2 [[1,0],[1,1]]) `shouldBe` Left (PieceExhausted (set [Ou]) 2)
      it "(1,0)(1,1)*1, (1,1)*2" $
        getLimitedUnpromoted ([[1,0],[1,1]]:replicate 2 [[1,1]]) `shouldBe` return (set[Ou])
      it "(1,0)(1,1)*1, (1,1)*3" $
        getLimitedUnpromoted ([[1,0],[1,1]]:replicate 3 [[1,1]]) `shouldBe` return (set[Ka,Gi,Ou])
      it "(1,0)(1,1)*1, (1,1)*4" $
        getLimitedUnpromoted ([[1,0],[1,1]]:replicate 4 [[1,1]]) `shouldBe` Left (PieceExhausted (set[Gi,Ka,Ou]) 5)
      it "(1,0)(1,1)*1, (0,1)*3" $
        getLimitedUnpromoted ([[1,0],[1,1]]:replicate 3 [[0,1]]) `shouldBe` return (set[Ki,Hi,Ou])
      it "(1,0)(1,1)*1, (0,1)*4" $
        getLimitedUnpromoted ([[1,0],[1,1]]:replicate 4 [[0,1]]) `shouldBe` Left (PieceExhausted (set[Ki,Hi,Ou]) 5)
    describe "simple detailed" $ do
      it "(0,-2)*2" $
        checkFromMoveUnpromoted [[[0,-2]],[[0,-2]]] `shouldBe` return [(set[],[0,1])]
      it "(0,-2)*2, (0,2)*1" $
        checkFromMoveUnpromoted [[[0,-2]],[[0,-2]],[[0,2]]] `shouldBe` return [(set[Hi],[2]),(set[],[0,1]),(set[Ky,Hi],[0,1,2])]
      it "(2,2)*2 (0,-2)*3" $
        checkFromMoveUnpromoted ([[2,2]]:replicate 3 [[0,-2]]) `shouldBe` return [(set [Ka],[0]), (set [Ky,Hi],[1,2,3]),(set[Ky,Ka,Hi],[1,2,3,0])]

    describe "ou detailed"$ do
      it "(1,0)(1,1)*1, (1,1)*2" $
        checkFromMoveUnpromoted ([[1,0],[1,1]]:replicate 2 [[1,1]]) `shouldBe` return [(fromList [Ou],[0]),(fromList [],[1,2]),(fromList [],[1,2,0])]
      it "(1,0)(1,1)*1, (1,1)*3" $
        checkFromMoveUnpromoted ([[1,0],[1,1]]:replicate 3 [[1,1]]) `shouldBe` return [(fromList [Ou],[0]),(fromList [],[1,2,3]),(fromList [Gi,Ka,Ou],[1,2,3,0])]

    describe "simple check" $ do
      it "(0,-2)*2" $
        checkUnpromoted [[[0,-2]],[[0,-2]]] `shouldBe` return ([set[Ky,Hi],set[Ky,Hi]], set[])
      it "(0,-2)*2, (0,2)*1" $
        checkUnpromoted [[[0,-2]],[[0,-2]],[[0,2]]] `shouldBe` return ([set[Ky],set[Ky],set[Hi]], set[Ky,Hi])
      it "(2,2) (0,-2)*3" $
        checkUnpromoted ([[2,2]]:replicate 3 [[0,-2]]) `shouldBe` return (set[Ka]:replicate 3 (set[Ky,Hi]), set[Ka,Ky,Hi])

    describe "ou check"$ do
      it "(1,0)(1,1)*1, (1,1)*2" $
        checkUnpromoted ([[1,0],[1,1]]:replicate 2 [[1,1]]) `shouldBe` return ([set[Ou],set[Gi,Ka],set[Gi,Ka]], set[Ou])
      it "(1,0)(1,1)*1, (1,1)*3" $
        checkUnpromoted ([[1,0],[1,1]]:replicate 3 [[1,1]]) `shouldBe` return ([set[Ou],set[Gi,Ka],set[Gi,Ka],set[Gi,Ka]], set[Ou,Gi,Ka])

    describe "ou gi ki check"$ do
      it "(2,2)*1, (1,1)(-1,-1)*1, (-1,-1)*4" $
        checkUnpromoted ([[2,2]]:replicate 1 [[1,1],[-1,-1]]++replicate 4 [[-1,-1]]) `shouldBe` return (set[Ka]:replicate 1(set[Gi,Ou])++replicate 4 (set[Gi,Ou,Ki]),set[Ka,Gi,Ki,Ou])
      it "(2,2)*1, (1,1)(-1,-1)*2, (-1,-1)*3" $
        checkUnpromoted ([[2,2]]:replicate 2 [[1,1],[-1,-1]]++replicate 3 [[-1,-1]]) `shouldBe` return (set[Ka]:replicate 2(set[Gi,Ou])++replicate 3 (set[Gi,Ou,Ki]),set[Ka,Gi,Ki,Ou])
      it "(2,2)*1, (1,1)(-1,-1)*3, (-1,-1)*2" $
        checkUnpromoted ([[2,2]]:replicate 3 [[1,1],[-1,-1]]++replicate 2 [[-1,-1]]) `shouldBe` return (set[Ka]:replicate 3(set[Gi,Ou])++replicate 2 (set[Ki]),set[Ka,Gi,Ki,Ou])

    describe "simple promote"$ do
      it "(0,-2)nari*1" $
        check [[([0,-2], True)]] `shouldBe` return ([set[Hi]], set[Hi])
      it "(1,1)nari(1,1)*1" $
        check [[([1,1], True),([1,1],False)]] `shouldBe` return ([set[Ka]], set[Ka])

    describe "simple step"$ do
      let a = step 0 ([0,-2], False) False []
      it "move 0 (0,-2)" $
        a `shouldBe` return ([set[Ky,Hi]],set[])
      let b = (liftM fst$ a) >>= step 1 ([0,-2], False) False 
      it "move 1 (0,-2)" $
        b `shouldBe` return (replicate 2$ set[Ky,Hi],set[])
      let c = (liftM fst$ b) >>= step 2 ([0,-2], False) False
      it "move 2 (0,-2)" $
        c `shouldBe` return (replicate 3$ set[Ky,Hi],set[Ky,Hi])
      let d = (liftM fst$ c) >>= step 2 ([0,2], False) False
      it "move 2 (0,2)" $
        d `shouldBe` return ((replicate 2$ set[Ky])++[set[Hi]],set[Ky,Hi])
      let e = (liftM fst$ d) >>= step 3 ([0,2], False) False
      it "move 3 (0,-2)" $
        e `shouldBe` Left (PieceExhausted (set[Hi]) 2)

    describe "promotenow"$ do
      let a = step 1 ([1,1], False) True [set[Ka]]
      it "move 1 (1,1)nari on 0=Ka" $
        a `shouldBe` return ([set[Ka],set[Gi]],set[Ka])

    describe "multi full"$ do
      let a = step 0 ([-2,-2], False) False []
      it "move 0 (-2,-2)" $
        a `shouldBe` return ([set[Ka]],set[Ka])
      let b = (liftM fst$ a) >>= step 1 ([0,-2], False) False 
      it "move 1 (0,-2)" $
        b `shouldBe` return ([set[Ka],set[Ky,Hi]],set[Ka])
      let c = (liftM fst$ b) >>= step 2 ([0,-2], False) False
      it "move 2 (0,-2)" $
        c `shouldBe` return (set[Ka]:(replicate 2$ set[Ky,Hi]),set[Ka])
      let d = (liftM fst$ c) >>= step 3 ([0,-2], False) False
      it "move 3 (0,-2)" $
        d `shouldBe` return (set[Ka]:(replicate 3$ set[Ky,Hi]),set[Ka,Ky,Hi])

    describe "gradual"$ do
      let a = step 1 ([1,1], False) False (set[Ka]:replicate 5 (set[Gi,Ki,Ou]))
      it "move 1 (1,1)nari on 0=Ka" $
        a `shouldBe` return (set[Ka]:set[Gi,Ou]:replicate 4 (set[Gi,Ki,Ou]),set[Ka,Gi,Ki,Ou])
