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

data MyError = InvalidMoveCombination Int
             | InvalidMove Int Int
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

initialPiecesMap :: Map Piece Int
initialPiecesMap = M.fromList initialPieces

movingBoard :: Map Move SuperPiece
movingBoard = Data.List.foldr (\(mv, p) mp->case M.lookup mv mp of
                                    Just set->M.insert mv (S.insert p set) mp
                                    Nothing->M.insert mv (S.insert p S.empty) mp) M.empty [(mov, piece)|(piece, num)<-initialPieces, mov<-move piece]

movingType :: [SuperPiece]
movingType = nub$ map snd$ M.toList movingBoard

showMap :: Map Move SuperPiece->String
showMap board = intercalate "\n" [show p++"\t"++(show$ S.toList mvs)|(p,mvs)<-M.toList board]

calcMax :: SuperPiece->Int
calcMax set = foldr1 (+)$ map (\p->case M.lookup p initialPiecesMap of Just x->x)$ S.toList set

subset :: [a]->[[a]]
subset = filterM$ const [False,True]
subsetNonEmpty =  tail.subset

maxs = map (\sup->(sup,calcMax sup)) movingUnions
maxsMap = M.fromList$ maxs

movingUnions :: [SuperPiece]
movingUnions = nub$ map (foldr1 S.union) movingTypeSubset

movingTypeSubset :: [[SuperPiece]]
movingTypeSubset = subsetNonEmpty movingType 

count :: (Eq a, Ord a)=>[a]->[(a,Int)]
count xs = map (\xs->(xs!!0, length xs))$Data.List.group$ Data.List.sort xs

countsUnion = map(foldr1 (\(s1,c1) (s2,c2)->(S.union s1 s2, c1+c2))). subsetNonEmpty

checkMaxFromUnion :: [(SuperPiece, Int)] -> ThrowsError Result
checkMaxFromUnion = liftM (foldr S.union S.empty). mapM (\(set, cnt)->case M.lookup set maxsMap of
                                                       Just mx->case cnt`compare`mx of 
                                                                  EQ->return set
                                                                  LT->return (S.empty)
                                                                  GT->throwError$ PieceExhausted set cnt)

checkMax :: [SuperPiece]->ThrowsError Result
checkMax supers = mapM f (zip supers [0..]) >>= checkMaxFromUnion. countsUnion. count
  where f (sup, i) = if sup==S.fromList[] then throwError$ InvalidMoveCombination i else return sup

checkMaxFromList :: [[Piece]]->ThrowsError Result
checkMaxFromList = checkMax. map S.fromList

checkMaxFromMove :: [[Move]]->ThrowsError Result
checkMaxFromMove moves = mapM (\(m,i)->superPieceFromMoves m i) (zip moves [0..]) >>= checkMax

superPieceFromMoves :: [Move]->Int->ThrowsError SuperPiece
superPieceFromMoves moves index = liftM (foldr1 S.intersection). mapM f$ zip moves [0..]
  where f (m,i) = case M.lookup m movingBoard of
                    Just sp->return sp
                    Nothing->throwError$ InvalidMove index i

checkMaxFromNums :: [([Move], Int)]->ThrowsError Result
checkMaxFromNums nums = superPieceFromNums nums >>= checkMax

superPieceFromNums :: [([Move], Int)]->ThrowsError [SuperPiece]
superPieceFromNums nums = liftM concat. mapM f$ zip nums [0..]
  where f ((mvs, num), i) = liftM(replicate num)$ superPieceFromMoves mvs i
