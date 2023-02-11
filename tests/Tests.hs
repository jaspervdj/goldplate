import System.Environment ( getArgs  )
import System.Exit        ( exitWith )
import System.Process     ( system   )

main :: IO ()
main = do
  args <- getArgs
  exitWith =<< do
    system $ unwords $ "goldplate tests" : args
