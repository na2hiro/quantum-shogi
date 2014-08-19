module Quantum where
import Data.Map as M hiding(map, filter, null, union, foldr, unions)
import Data.Set as S hiding(map, filter, foldr)
import Data.List --(intercalate, nub, foldr1, union, foldr, group)
import Control.Monad(filterM)
import Control.Monad.Error

data Piece = Fu | Ky | Ke | Gi | Ki | Ka | Hi | Ou deriving (Eq, Ord, Show, Read)
data MoveType = Promote | NoPromote1 | NoPromote2 | Captured | Normal deriving(Eq, Show, Read)
type Move = [Int]
type SuperPiece = Set Piece
type Result = ([SuperPiece], SuperPiece)
type Possibilities = [(SuperPiece, [Index])]
type Index = Int
type Log = (Move, Promote)
type Promote = Bool

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

kin :: [Move]
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

filteringPiece :: MoveType->[Piece]
filteringPiece Promote = [Ki,Ou]
filteringPiece NoPromote1 = [Fu,Ky,Ke]
filteringPiece NoPromote2 = [Ke]
filteringPiece Captured = [Ou]
filteringPiece Normal = []

allPieces :: SuperPiece
allPieces = S.fromList[Fu,Ky,Ke,Gi,Ki,Ka,Hi,Ou]

filterByType :: MoveType->SuperPiece->SuperPiece
filterByType mt sp = sp `S.difference` S.fromList (filteringPiece mt)

initialPieces :: [(Piece, Int)]
initialPieces = [(Fu, 9), (Ky, 2), (Ke, 2), (Gi, 2), (Ki, 2), (Ka, 1), (Hi, 1), (Ou, 1)]

movingBoard :: Bool->Map Move SuperPiece
movingBoard prom = Data.List.foldr f M.empty [(mov, piece)|(piece, _)<-initialPieces, mov<-move piece prom]
  where f (mv, p) mp = case M.lookup mv mp of
                         Just set->M.insert mv (S.insert p set) mp
                         Nothing->M.insert mv (S.insert p S.empty) mp

showMap :: Map Move SuperPiece->String
showMap board = intercalate "\n" [show p++"\t"++(show$ S.toList mvs)|(p,mvs)<-M.toList board]

calcMax :: SuperPiece->Int
calcMax set = foldr(+)0$ map snd$ filter (\(p, _)->S.member p set) initialPieces

powerset :: [a]->[[a]]
powerset = filterM$ const [False,True]
powersetNonEmpty :: [a]->[[a]]
powersetNonEmpty = tail.powerset

countWithIndices :: (Eq a, Ord a)=>[a]->[(a,[Index])]
countWithIndices = map (\xs->(fst(xs!!0), map snd xs)). groupBy fstEq. sortBy fstOrd. flip zip [0..]
  where fstOrd (a,_) (b,_) = compare a b
        fstEq (a,_) (b,_) = a==b

countsUnionWithIndices :: Possibilities->Possibilities
countsUnionWithIndices = map(foldr1 (\(s1,l1) (s2,l2)->(S.union s1 s2, l1++l2))). powersetNonEmpty

foldPossibilities :: Possibilities->SuperPiece
foldPossibilities = unions. map fst

checkPossibilities :: Possibilities->ThrowsError Possibilities
checkPossibilities = mapM f
  where f (set, is) = let cnt=length is in case cnt `compare` calcMax set of 
                         EQ->return (set, is)
                         LT->return (S.empty, is)
                         GT->throwError$ PieceExhausted set cnt

checkFromSuperPiece :: [SuperPiece]->ThrowsError Possibilities
checkFromSuperPiece sps = superPiece2union sps >>= checkPossibilities

checkFromMove ::  [[Log]]->ThrowsError Possibilities
checkFromMove logs = moves2superPieces logs >>= checkFromSuperPiece

checkFromMoveUnpromoted ::  [[Move]]->ThrowsError Possibilities
checkFromMoveUnpromoted = checkFromMove. map enpromote

getLimitedUnpromoted :: [[Move]]->ThrowsError SuperPiece
getLimitedUnpromoted = liftM foldPossibilities. checkFromMove. map enpromote

enpromote :: [Move]->[Log]
enpromote = flip zip (repeat False)

-- 誰をも含まない駒集合から固定していく
assign1 :: Possibilities->Maybe ((SuperPiece, [Index]), Possibilities)
assign1 xs = do
    x@(sp, is) <- find (\(x,_)->all(\(xx,_)->x==xx||not(xx `isSubsetOf` x))xs) xs
    return (x, map (\(sp1, is1)->((S.\\)sp1 sp, (Data.List.\\) is1 is))$ filter(/=x)xs)

assign :: Possibilities->(Possibilities, Possibilities)
assign xs = case assign1 xs' of
              Nothing->([], xs')
              Just (p, others)->let (ys, remain)=assign others in (p:ys, remain)
  where xs' = filter(not. S.null. fst) xs

assign2list :: Int->(Possibilities, Possibilities) -> [[Piece]]
assign2list len (x, y)= a2l 0$ sortBy compareSnd$ unwind(x++y)
  where a2l n [] = replicate (len-n) []
        a2l n xss@((sp,i):xs)|n==i = S.toList sp:a2l (n+1) xs
                           |otherwise = []:a2l (n+1) xss
        unwind xs = nub. concat. map (\(sp, is)->[(sp, i)|i<-is])$ xs
        compareSnd (_,i1) (_,i2) = compare i1 i2

check :: [[Log]]->ThrowsError Result
check lgs = do
    sps <- moves2superPieces lgs
    getResult sps

getResult :: [SuperPiece]->ThrowsError Result
getResult sps = do
    detail <- checkFromSuperPiece sps
    let ps = assign2list (length sps). assign$ detail
    let fulls = unions$ map fst detail
    return (map(\(p, sp)->if p==[] then ((S.\\) sp fulls) else (S.fromList p)`S.intersection`sp)$ zip ps sps, fulls)

checkUnpromoted :: [[Move]]->ThrowsError([SuperPiece], SuperPiece)
checkUnpromoted = check. map enpromote

-- utils

move2superPiece :: (Index,[Log])->ThrowsError SuperPiece
move2superPiece (index, logs) = liftM (foldr1 S.intersection). mapM f$ zip [0..] logs
  where f (i,(m, prom)) = case M.lookup m (movingBoard prom) of
                    Just sp->return sp
                    Nothing->throwError$ InvalidMove index i

moves2superPieces :: [[Log]]->ThrowsError [SuperPiece]
moves2superPieces = mapM move2superPiece. zip [0..]

superPiece2union :: [SuperPiece]->ThrowsError Possibilities 
superPiece2union supers = liftM (countsUnionWithIndices. countWithIndices)$ mapM f (zip [0..] supers) 
  where f (i, sup) = if sup==S.fromList[] then throwError$ InvalidMoveCombination i else return sup

applyNth :: Int->(a->a)->[a]->[a]
applyNth n f xs = take n xs ++ f (xs!!n) : drop (n+1) xs

step :: Int->Maybe Log->MoveType->[SuperPiece]->ThrowsError ([SuperPiece], SuperPiece)
step iden maybelg movetype sps = do
    super <- case maybelg of
               Just lg->move2superPiece (0, [lg])
               Nothing->return allPieces
    let sp = filterByType movetype super
    let newsps = if iden<length sps
                   then applyNth iden (S.intersection sp) sps
                   else sps++[sp]
    getResult newsps
