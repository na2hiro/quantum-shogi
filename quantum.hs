module Quantum where
import Data.Map as M hiding(map, filter, null, union, foldr)
import Data.Set as S hiding(map, filter, foldr)
import Data.List --(intercalate, nub, foldr1, union, foldr, group)
import Control.Monad(filterM)
import Control.Monad.Error

data Piece = Fu | Ky | Ke | Gi | Ki | Ka | Hi | Ou deriving (Eq, Ord)
type Move = [Int]
type SuperPiece = Set Piece
type Result = SuperPiece
type DetailedResult = [(SuperPiece, [Index])]
type Index = Int
type Log = (Move, Promote)
type Promote = Bool

showPiece Fu = "aa"
showPiece Ky = "ab"
showPiece Ke = "ac"
showPiece Gi = "ad"
showPiece Ki = "ae"
showPiece Ka = "af"
showPiece Hi = "ag"
showPiece Ou = "ah"

instance Show Piece where show = showPiece

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

kin = [[1,-1],[0,-1],[-1,-1],[1,0],[-1,0],[0,1]]
move :: Piece->Bool->[Move]
move Fu False = [[0,-1]]
move Ky False = [[0,-1],[0,-2]]
move Ke False = [[-1,-2],[1,-2]]
move Gi False = [[1,-1],[0,-1],[-1,-1],[1,1],[-1,1]]
move Ki False = [[1,-1],[0,-1],[-1,-1],[1,0],[-1,0],[0,1]]
move Ka False = [[1,-1],[-1,-1],[1,1],[-1,1],[2,-2],[-2,-2],[2,2],[-2,2]]
move Hi False = [[1,0],[-1,0],[0,1],[0,-1],[2,0],[-2,0],[0,2],[0,-2]]
move Ou False = [[1,-1],[-1,-1],[1,1],[-1,1],[1,0],[-1,0],[0,1],[0,-1]]
move Fu True = kin
move Ky True = kin 
move Ke True = kin 
move Gi True = kin 
move Ki True = []
move Ka True = [[1,0],[-1,0],[0,1],[0,-1],[1,-1],[-1,-1],[1,1],[-1,1],[2,-2],[-2,-2],[2,2],[-2,2]]
move Hi True = [[1,-1],[-1,-1],[1,1],[-1,1],[1,0],[-1,0],[0,1],[0,-1],[2,0],[-2,0],[0,2],[0,-2]]
move Ou True = []

initialPieces :: [(Piece, Int)]
initialPieces = [(Fu, 9), (Ky, 2), (Ke, 2), (Gi, 2), (Ki, 2), (Ka, 1), (Hi, 1), (Ou, 1)]

movingBoard :: Bool->Map Move SuperPiece
movingBoard prom = Data.List.foldr f M.empty [(mov, piece)|(piece, num)<-initialPieces, mov<-move piece prom]
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

checkMaxDetailedFromMove ::  [[Log]]->ThrowsError DetailedResult
checkMaxDetailedFromMove logs = moves2superPieces logs >>= superPiece2union >>= checkMaxDetailedFromUnion

checkMaxDetailedFromMoveUnpromoted ::  [[Move]]->ThrowsError DetailedResult
checkMaxDetailedFromMoveUnpromoted = checkMaxDetailedFromMove. map enpromote

checkMaxDetailedFromNums ::  [([Log], Int)]->ThrowsError DetailedResult
checkMaxDetailedFromNums nums = moves2superPieces (nums2moves nums)>>= superPiece2union >>= checkMaxDetailedFromUnion

checkMaxDetailedFromNumsUnpromoted ::  [([Move], Int)]->ThrowsError DetailedResult
checkMaxDetailedFromNumsUnpromoted = checkMaxDetailedFromNums. map (\(mvs, i)->(enpromote mvs, i))

checkMaxFromSuperPiece :: [SuperPiece]->ThrowsError Result
checkMaxFromSuperPiece supers = superPiece2union supers >>= checkMaxFromUnion

checkMaxFromMove :: [[Log]]->ThrowsError Result
checkMaxFromMove logs = moves2superPieces logs >>= checkMaxFromSuperPiece

checkMaxFromMoveUnpromoted :: [[Move]]->ThrowsError Result
checkMaxFromMoveUnpromoted moves = checkMaxFromMove$ map enpromote moves

checkMaxFromNums :: [([Log], Int)]->ThrowsError Result
checkMaxFromNums = checkMaxFromMove. nums2moves

checkMaxFromNumsUnpromoted :: [([Move], Int)]->ThrowsError Result
checkMaxFromNumsUnpromoted = checkMaxFromMove. nums2moves. map (\(ms, i)->(enpromote ms, i))

enpromote :: [Move]->[Log]
enpromote = flip zip (repeat False)

assign1 :: [(SuperPiece, [Index])]->Maybe ((SuperPiece, [Index]), [(SuperPiece, [Index])])
assign1 xs = do
    x@(sp, is) <- find ((==1). S.size. fst)$ xs
    return (x, map (\(sp1, is1)->((S.\\)sp1 sp, (Data.List.\\) is1 is))$ filter(/=x)xs)

assign :: [(SuperPiece, [Index])]->([(SuperPiece, [Index])], [(SuperPiece, [Index])])
assign xs = case assign1 xs' of
              Nothing->([], xs')
              Just (pi, others)->let (ys, remain)=assign others in (pi:ys, remain)
  where xs' = filter(not. S.null. fst) xs

assign2list :: Int->([(SuperPiece, [Index])], [(SuperPiece, [Index])]) -> [[Piece]]
assign2list len (x, y)= a2l 0$ sortBy(\(_,i1) (_,i2)->compare i1 i2)$ unwind(x++y)
  where a2l n [] = replicate (len-n) []
        a2l n xss@(x@(sp,i):xs)|n==i = S.toList sp:a2l (n+1) xs
                           |otherwise = []:a2l (n+1) xss
        unwind xs = concat. map (\(sp, is)->[(sp, i)|i<-is])$ xs

check :: [[Log]]->ThrowsError([SuperPiece], SuperPiece)
check lgs = do
    sps <- moves2superPieces lgs
    detail <- checkMaxDetailedFromMove lgs
    let ps = assign2list (length lgs). assign$ detail
    let fulls = foldr S.union S.empty$ map fst detail
    return (map(\(p, sp)->if p==[] then ((S.\\) sp fulls) else S.fromList p)$ zip ps sps, fulls)

checkUnpromoted :: [[Move]]->ThrowsError([SuperPiece], SuperPiece)
checkUnpromoted = check. map enpromote

checkFromNums :: [([Log], Int)]->ThrowsError([SuperPiece],SuperPiece)
checkFromNums = check. nums2moves

checkFromNumsUnpromoted :: [([Move], Int)]->ThrowsError([SuperPiece],SuperPiece)
checkFromNumsUnpromoted = checkFromNums. map (\(mvs, i)->(enpromote mvs, i))

-- utils

nums2moves :: [([Log], Int)]->[[Log]]
nums2moves = concat. map (\(mvs, n)->replicate n mvs)

move2superPiece :: (Index,[Log])->ThrowsError SuperPiece
move2superPiece (index, logs) = liftM (foldr1 S.intersection). mapM f$ zip [0..] logs
  where f (i,(m, prom)) = case M.lookup m (movingBoard prom) of
                    Just sp->return sp
                    Nothing->throwError$ InvalidMove index i

moves2superPieces :: [[Log]]->ThrowsError [SuperPiece]
moves2superPieces = mapM move2superPiece. zip [0..]

superPiece2union :: [SuperPiece]->ThrowsError [(SuperPiece, [Index])]
superPiece2union supers = liftM (countsUnionWithIndices. countWithIndices)$ mapM f (zip [0..] supers) 
  where f (i, sup) = if sup==S.fromList[] then throwError$ InvalidMoveCombination i else return sup
