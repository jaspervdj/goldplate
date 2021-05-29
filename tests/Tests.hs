import System.Exit     ( exitWith  )
import System.Process  ( system    )
import System.FilePath ( (</>)     )

import Paths_goldplate ( getBinDir )

main :: IO ()
main = do
  bindir <- getBinDir
  exitWith =<< system ("goldplate tests")
