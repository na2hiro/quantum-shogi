{-# LANGUAGE DeriveDataTypeable #-}
import Quantum
import Data.Typeable
import Text.JSON
import Text.JSON.Generic
import System.Environment(getArgs)
import Data.Set as S hiding(map)
main = do
    args<-getArgs
    let id = read(args!!0)::Int
    let mov = read(args!!1)::Move
    let promoted = read(args!!2)::Promote
    let promotenow = read(args!!3)::Promote
    let poss = read(args!!4)::[SuperPiece]
    let next = step id (mov, promoted) promotenow poss
    putStrLn$ encodeJSON$ toJ$ next

enc = encodeJSON Json{err=False, message="hoge",native="",kinds=[],fulls=[]}

--toJ :: Data a=>ThrowsError([SuperPiece], SuperPiece)->a
toJ (Left a) = Json{err=True, message=show a,native="",kinds=[],fulls=[]}
toJ (Right (sps, fulls)) = Json{err=False
                               , message=""
                               , native=show sps
                               , kinds=map(map show. S.toList) sps
                               , fulls=map show$ S.toList fulls}

data Json = Json{
          err :: Bool,
          message :: String,
          native :: String,
          kinds :: [[String]],
          fulls :: [String]} deriving(Typeable, Data)

