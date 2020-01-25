import qualified Data.Text.IO as TIO
import System.Environment (getArgs)
import System.Exit (die, exitSuccess)

import Dimacs (parseDimacsCnf)
import Suma (sat)

main = do
  args <- getArgs
  case args of
    [] -> do
      die "Usage: suma [FILE]"
    dimacsFile:_ -> do
      dimacsText <- TIO.readFile dimacsFile
      case parseDimacsCnf dimacsText of
        Nothing -> do
          die "Failed to parse DIMACS file"
        Just formula ->
          putStrLn $ if sat formula then "SAT" else "UNSAT"
