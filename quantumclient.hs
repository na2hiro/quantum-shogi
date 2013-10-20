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
    let lg = read(args!!1)::Maybe Log
    let movetype = read(args!!2)::MoveType
    let poss = read(args!!3)::[SuperPiece]
    let next = step id lg movetype poss
    putStrLn$ encodeJSON$ toJ$ next

--enc = encodeJSON Json{err=False, message="hoge",native="",kinds=[],fulls=[]}

toJ :: ThrowsError([SuperPiece], SuperPiece)->Json
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

