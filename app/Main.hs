{-# LANGUAGE OverloadedStrings #-}
module Main where
import System.Environment (getArgs)
import Text.Megaparsec (parse)
import Types
import Parser
import Compiler
import Stats
import Options.Applicative 
import Data.Text (replace)
import Control.Exception (try, IOException)
import Data.Text (Text)
import qualified Data.Text.IO as TIO

--Some things that should be options:
-- Name of Next, Name of Spec, Name of vars, Name of Init

-- options parsing stuff
data PropOptions = 
    FileInput String
      | ArgInput String

data CmdOptions = 
    CmdOpt { 
            propOpts  :: PropOptions,
            inModule  :: String,
            outModule :: String,
            delta     :: Double,
            alpha     :: Double,
            beta      :: Double
           }

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
    <*> option auto
         (  long "delta"
         <> short 'd'
         <> metavar "PROBABILITY"
         <> help "The range of indifference for the sampling plan"
         <> value 0.005 )
    <*> option auto
         (  long "alpha"
         <> short 'a'
         <> metavar "PROBABILITY"
         <> help "The acceptable likelihood of false positive for the sampling plan"
         <> value 0.001 )
    <*> option auto
         (  long "beta"
         <> short 'b'
         <> metavar "PROBABILITY"
         <> help "The acceptable likelihood of false positive for the sampling plan"
         <> value 0.001 )


opts :: ParserInfo CmdOptions 
opts = info (cmdOpts <**> helper)
  ( fullDesc
  <> header "pbltlc - A transpiler from Probabalistic Bounded Linear Temporal Logic (of actions) to TLA+" )

-- needs to be seperate, otherwise we get a typechecking error 
tryToReadFile :: String -> IO (Either IOException Text)
tryToReadFile s = try $ TIO.readFile s

main :: IO ()
main = do
    options <- execParser opts
    formOpt <- case propOpts options of
                 FileInput s -> parseString =<< (readFile s)
                 ArgInput s  -> parseString s
    putStrLn $ "Parsed: " ++ show formOpt 
    let form = case formOpt of
                 Left (Pr _ f) -> f
                 Right f       -> f
    case formOpt of
        Left (Pr p f) -> do
            putStrLn "Single Sampling Plan (required runs,required acceptances):"
            print (samplingPlan (p - (delta options)) (p + (delta options)) (alpha options) (beta options))
        Right _       -> pure ()
    writeFile ((outModule options) ++ ".tla") 
        $ genModule (outModule options) (inModule options) "vars" form
    cfg' <- tryToReadFile ((inModule options) ++ ".cfg")
    case cfg' of
        Left _ -> pure ()
        Right cfg -> TIO.writeFile ((outModule options) ++ ".cfg") 
            (replace "\tSpec" "\tNewStatisticalSpec" (replace " Spec" " NewStatisticalSpec" cfg))

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
