import qualified Data.Text.IO as TIO
import System.Environment (getArgs)
import System.Exit (exitFailure, exitSuccess)

import Dimacs (parseDimacsCnf)
import Suma (sat)

main = do
  args <- getArgs
  case args of
    [] -> do
      putStrLn "Usage: suma [FILE]"
      exitFailure
    dimacsFile:_ -> do
      dimacsText <- TIO.readFile dimacsFile
      case parseDimacsCnf dimacsText of
        Nothing -> do
          putStrLn "Failed to parse DIMACS file"
          exitFailure
        Just formula ->
          putStrLn $ if sat formula then "SAT" else "UNSAT"
