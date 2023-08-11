module Main where
import System.Environment (getArgs)
import Text.Megaparsec (parse)
import Types
import Parser
import Compiler
import Options.Applicative 

-- options parsing stuff
data PropOptions = 
    FileInput String
      | ArgInput String

data CmdOptions = 
    CmdOpt { fileOpts  :: PropOptions,
      inModule  :: String,
      outModule :: String }


cmdOpts :: Parser CmdOptions
cmdOpts = CmdOpt
    <$> propertyInput 
    <*> strOption 
         (  long "input"
         <> short 'i'
         <> metavar "TLA-MODULE"
         <> help "The base module you want to verify statistical properties of" )
    <*> strOption 
         (  long "output"
         <> short 'o'
         <> metavar "TLA-MODULE"
         <> help "The name of the module" )

fileInput :: Parser PropOptions 
fileInput = FileInput <$> (strOption
  (  long "file"
  <> short 'f'
  <> metavar "FILENAME"
  <> help "A file containing the PBLTL property you want to verify" ))

argInput :: Parser PropOptions
argInput = ArgInput <$> (argument str
    (  metavar "\"PBLTL-PROPERTY\""
    <> help "A string containing the PBLTL property you want to verify"
    ))

propertyInput :: Parser PropOptions 
propertyInput = fileInput <|> argInput

opts :: ParserInfo CmdOptions 
opts = info (cmdOpts <**> helper)
  ( fullDesc
  <> header "pbltlc - A transpiler from Probabalistic Bounded Linear Temporal Logic (of actions) to TLA+" )

main :: IO ()
main = do
    options <- execParser opts
    form <- case fileOpts options of
                 FileInput s -> parseString =<< (readFile s)
                 ArgInput s  -> parseString s

    writeFile ((outModule options) ++ ".tla") 
        $ genModule (outModule options) (inModule options) "vars" form

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
