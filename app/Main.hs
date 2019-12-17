import System.Environment
import System.Exit

import Parser
import Proof

main :: IO ()
main = getArgs >>= run

run []     = usage   >> exitWith ExitSuccess
run ["-h"] = usage   >> exitWith ExitSuccess
run ["-v"] = version >> exitWith ExitSuccess
run [file] = do text <- readFile file
                let (sig, ctxt, proof) = parse text
                let result             = checkProof ctxt proof
                putStrLn $ show $ result
                if result == Correct then
                  exitWith ExitSuccess
                else
                  exitWith (ExitFailure 1)
run _      = usage   >> exitWith ExitSuccess

usage   = putStrLn "Usage: fol-proof-checker [-vh] [file]"
version = putStrLn "fol-proof-checker 1.0.0"
