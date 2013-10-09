module Quantum where
import Data.Map as M hiding(map, filter, null, union, foldr)
import Data.Set as S hiding(map, filter, null, foldr)
import Data.List --(intercalate, nub, foldr1, union, foldr, group)
import Control.Monad(filterM)
import Control.Monad.Error

data Piece = Fu | Ky | Ke | Gi | Ki | Ka | Hi | Ou deriving (Show, Eq, Ord)
type Move = [Int]
type SuperPiece = Set Piece
type Result = SuperPiece
type DetailedResult = [(SuperPiece, [Index])]
type Index = Int

data MyError = InvalidMoveCombination Index
             | InvalidMove Index Index 
             | PieceExhausted SuperPiece Int
             | Default String deriving(Eq)

instance Error MyError where
    noMsg = Default "some error"
    strMsg = Default

showError :: MyError->String
showError (InvalidMove p m) = "invalid move: "++show p++", "++show m
showError (InvalidMoveCombination p) = "invalid move combination: "++show p
showError (PieceExhausted super num) = "piece exhausted: "++show (S.toList super)++show num
showError (Default str) = str

instance Show MyError where show = showError

type ThrowsError = Either MyError

move :: Piece->[Move]
move Fu = [[0,-1]]
move Ky = [[0,-1],[0,-2]]
move Ke = [[-1,-2],[1,-2]]
move Gi = [[1,-1],[0,-1],[-1,-1],[1,1],[-1,1]]
move Ki = [[1,-1],[0,-1],[-1,-1],[1,0],[-1,0],[0,1]]
move Ka = [[1,-1],[-1,-1],[1,1],[-1,1],[2,-2],[-2,-2],[2,2],[-2,2]]
move Hi = [[1,0],[-1,0],[0,1],[0,-1],[2,0],[-2,0],[0,2],[0,-2]]
move Ou = [[1,-1],[-1,-1],[1,1],[-1,1],[1,0],[-1,0],[0,1],[0,-1]]

initialPieces :: [(Piece, Int)]
initialPieces = [(Fu, 9), (Ky, 2), (Ke, 2), (Gi, 2), (Ki, 2), (Ka, 1), (Hi, 1), (Ou, 1)]

movingBoard :: Map Move SuperPiece
movingBoard = Data.List.foldr f M.empty [(mov, piece)|(piece, num)<-initialPieces, mov<-move piece]
  where f (mv, p) mp = case M.lookup mv mp of
                         Just set->M.insert mv (S.insert p set) mp
                         Nothing->M.insert mv (S.insert p S.empty) mp

showMap :: Map Move SuperPiece->String
showMap board = intercalate "\n" [show p++"\t"++(show$ S.toList mvs)|(p,mvs)<-M.toList board]

calcMax :: SuperPiece->Int
calcMax set = foldr(+)0$ map snd$ filter (\(p, num)->S.member p set) initialPieces

subset :: [a]->[[a]]
subset = filterM$ const [False,True]
subsetNonEmpty =  tail.subset

countWithIndices :: (Eq a, Ord a)=>[a]->[(a,[Index])]
countWithIndices xs = map (\xs->(fst(xs!!0), map snd xs))$groupBy fstEq$ sortBy fstOrd$ zip xs [0..]
  where fstOrd (a,_) (b,_) = compare a b
        fstEq (a,_) (b,_) = a==b

countsUnionWithIndices = map(foldr1 (\(s1,l1) (s2,l2)->(S.union s1 s2, l1++l2))). subsetNonEmpty

checkMaxFromUnion :: [(SuperPiece, [Index])] -> ThrowsError Result
checkMaxFromUnion = liftM (foldr S.union S.empty). liftM (map fst). checkMaxDetailedFromUnion

checkMaxDetailedFromUnion :: [(SuperPiece, [Index])]->ThrowsError [(SuperPiece, [Index])]
checkMaxDetailedFromUnion = mapM f
  where f (set, is) = let cnt=length is in case cnt `compare` calcMax set of 
                         EQ->return (set, is)
                         LT->return (S.empty, is)
                         GT->throwError$ PieceExhausted set cnt

checkMaxDetailedFromMove ::  [[Move]]->ThrowsError DetailedResult
checkMaxDetailedFromMove moves = moves2superPieces moves >>= superPiece2union >>= checkMaxDetailedFromUnion

checkMaxDetailedFromNums ::  [([Move], Int)]->ThrowsError DetailedResult
checkMaxDetailedFromNums nums = moves2superPieces (nums2moves nums)>>= superPiece2union >>= checkMaxDetailedFromUnion

checkMaxFromSuperPiece :: [SuperPiece]->ThrowsError Result
checkMaxFromSuperPiece supers = superPiece2union supers >>= checkMaxFromUnion

checkMaxFromMove :: [[Move]]->ThrowsError Result
checkMaxFromMove moves = moves2superPieces moves >>= checkMaxFromSuperPiece

checkMaxFromNums :: [([Move], Int)]->ThrowsError Result
checkMaxFromNums = checkMaxFromMove. nums2moves

-- utils

nums2moves :: [([Move], Int)]->[[Move]]
nums2moves = concat. map (\(mvs, n)->replicate n mvs)

move2superPiece :: (Index,[Move])->ThrowsError SuperPiece
move2superPiece (index, moves) = liftM (foldr1 S.intersection). mapM f$ zip [0..] moves
  where f (i,m) = case M.lookup m movingBoard of
                    Just sp->return sp
                    Nothing->throwError$ InvalidMove index i

moves2superPieces :: [[Move]]->ThrowsError [SuperPiece]
moves2superPieces = mapM move2superPiece. zip [0..]

superPiece2union :: [SuperPiece]->ThrowsError [(SuperPiece, [Index])]
superPiece2union supers = liftM (countsUnionWithIndices. countWithIndices)$ mapM f (zip [0..] supers) 
  where f (i, sup) = if sup==S.fromList[] then throwError$ InvalidMoveCombination i else return sup
