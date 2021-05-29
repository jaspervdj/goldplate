import System.Exit    ( exitWith )
import System.Process ( system )

main :: IO ()
main = exitWith =<< system "cabal run -- goldplate tests"
