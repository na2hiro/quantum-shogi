{-# LANGUAGE DeriveDataTypeable #-}
import Quantum
import Data.Typeable
import Text.JSON
import Text.JSON.Generic
import System.Environment(getArgs)
import Data.Set as S hiding(map)
main = do
    args<-getArgs
    putStrLn$ encodeJSON$ toJ$ check(read(concat args)::[[Move]])

enc = encodeJSON Json{err=False, message="hoge",kinds=[],fulls=[]}

--toJ :: Data a=>ThrowsError([SuperPiece], SuperPiece)->a
toJ (Left a) = Json{err=True, message=show a,kinds=[],fulls=[]}
toJ (Right (sps, fulls)) = Json{err=False
                               , message=""
                               , kinds=map(map show. S.toList) sps
                               , fulls=map show$ S.toList fulls}

data Json = Json{
          err :: Bool,
          message :: String,
          kinds :: [[String]],
          fulls :: [String]} deriving(Typeable, Data)

