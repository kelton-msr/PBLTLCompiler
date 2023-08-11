module Main where
import System.Environment (getArgs)
import Text.Megaparsec (parse)
import Types
import Parser
import Compiler
import Options.Applicative 

-- options parsing stuff
data CmdOptions = 
    FileInput String
      | ArgInput String

fileInput :: Parser CmdOptions 
fileInput = FileInput <$> (strOption
  (  long "file"
  <> short 'f'
  <> metavar "FILENAME"
  <> help "A file containing the PBLTL property you want to verify" ))

argInput :: Parser CmdOptions
argInput = ArgInput <$> (argument str
    (  metavar "\"PBLTL-PROPERTY\""
    <> help "A string containing the PBLTL property you want to verify"
    ))

input :: Parser CmdOptions 
input = fileInput <|> argInput

opts :: ParserInfo CmdOptions
opts = info (input <**> helper)
  ( fullDesc
  <> header "pbltlc - A transpiler from Probabalistic Bounded Linear Temporal Logic (of actions) to TLA+" )

main :: IO ()
main = do
    options <- execParser opts
    form <- case options of
                 FileInput s -> parseString =<< (readFile s)
                 ArgInput s  -> parseString s
    putStrLn $ "VARIABLES isViolated, " ++ c
    let prop = compileProperty form
    putStrLn  "NextP == "
    putStrLn $ prettyPrintTForms $ propertyToCSVNext prop ++ (collateAssignments (c ++ "'") $ compileNext form)
    putStrLn  "InitP == "
    putStrLn $ prettyPrintTForms $ collateAssignments c $ compileInit form
    putStrLn "\\* Note that we cannot use this property for verifcation due to a bug in TLC"
    putStrLn  "Property == "
    putStrLn $ "    " ++ (show $ prop)
    pure ()

--- examples
example :: LTLForm
example = LBox ((LAtom "q") `LImplies` LBDiamond 20 (LAtom "p"))
example2 :: LTLForm
example2 = LBox ((LAtom "q") `LImplies` LBDiamond 20 (LBox $ LAtom "p"))
example3 :: LTLForm
example3 = LBox ((LAtom "q") `LAnd` LBDiamond 20 (LBox $ LAtom "p"))

d1 :: LTLForm
d1 = LBDiamond 20 (LAtom "p")
d2 :: LTLForm
d2 = LBDiamond 20 (LAtom "q" `LImplies` LAtom "p")
d3 :: LTLForm
d3 = LBDiamond 25 $ LBDiamond 20 $ LAtom "p"
d4 :: LTLForm
d4 = LAtom "q" `LImplies` (LBDiamond 20 $ LAtom "r")
