{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE LambdaCase #-}

module Efg where

import Options.Applicative (Parser, ParserInfo)
import Prettyprinter (Doc)
import Prettyprinter.Render.Terminal (AnsiStyle)
import System.Console.Terminal.Size (Window (..))

import qualified Data.Text.IO as Text.IO
import qualified Efg.Parser as Parser
import qualified Efg.Pretty
import qualified Efg.Syntax as Syntax
import qualified Options.Applicative as Options
import qualified Options.Applicative as Opts
import qualified Prettyprinter as Pretty
import qualified System.Console.ANSI as ANSI
import qualified System.Console.Terminal.Size as Size

data Highlight
  = -- | Force the use of ANSI color escape sequences to highlight source code
    Color
  | -- | Don't highlight source code
    Plain
  | -- | Auto-detect whether to use ANSI color escape sequences to highlight source code
    --   based on whether the @stdout@ is a terminal
    Auto

data Options
  = Interpret {file :: FilePath, highlight :: Highlight}
  | Parse {file :: FilePath, highlight :: Highlight}
  | Format {file :: FilePath, highlight :: Highlight}

parserInfo :: ParserInfo Options
parserInfo =
  Opts.info
    (Opts.helper <*> parser)
    ( Opts.fullDesc
        <> Opts.progDesc "Efg interpreter"
        <> Opts.header "efg - Efg interpreter"
    )

parser :: Parser Options
parser = do
  let interpret = do
        file <-
          Opts.strArgument
            ( Opts.help "File to interpret"
                <> Opts.metavar "FILE"
            )
        highlight <- parseHighlight
        return Interpret {..}

  let parse = do
        file <-
          Opts.strArgument
            ( Opts.help "File to parse"
                <> Opts.metavar "FILE"
            )
        highlight <- parseHighlight
        return Parse {..}

  let format = do
        file <-
          Opts.strArgument
            ( Options.help "File to format"
                <> Options.metavar "FILE"
            )
        highlight <- parseHighlight
        return Format {..}

  Opts.hsubparser
    ( Opts.command
        "interpret"
        ( Opts.info
            interpret
            (Opts.progDesc "Interpret a file")
        )
        <> Opts.command
          "parse"
          ( Opts.info
              parse
              (Opts.progDesc "Parse a file")
          )
        <> Opts.command
          "format"
          ( Opts.info
              format
              (Opts.progDesc "Format a file")
          )
    )
  where
    parseHighlight =
      Options.flag' Color (Options.long "color" <> Options.help "Enable syntax highlighting")
        <|> Options.flag' Plain (Options.long "plain" <> Options.help "Disable syntax highlighting")
        <|> pure Auto

detectColor :: Highlight -> IO Bool
detectColor = \case
  Color -> return True
  Plain -> return False
  Auto -> ANSI.hSupportsANSI stdout

getWidth :: IO Int
getWidth =
  Size.size >>= \case
    Nothing -> return 80
    Just Window {..} -> return width

getRender :: Highlight -> IO (Doc AnsiStyle -> IO ())
getRender highlight = do
  color <- detectColor highlight
  width <- getWidth
  return (Efg.Pretty.renderIO color width stdout)

getCode :: FilePath -> IO Text
getCode = \case
  "-" -> Text.IO.getContents
  fp -> decodeUtf8 <$> readFileBS fp

parse_ :: FilePath -> Highlight -> IO ()
parse_ filepath highlight = do
  code <- getCode filepath
  let module' = Parser.parseModule filepath code
  let errors = Syntax.moduleErrors module'

  unless (null errors) $ do
    Text.IO.hPutStrLn stdout "Parse errors:\n"
    forM_ errors $ \err -> do
      Text.IO.hPutStr stderr (toText err)
      Text.IO.hPutStr stderr "\n"
    Text.IO.hPutStrLn stdout "------------------\n"

  render <- getRender highlight
  render (Efg.Pretty.pretty module' <> Pretty.hardline)

formatFile :: FilePath -> Highlight -> IO ()
formatFile filepath highlight = do
  code <- getCode filepath
  let module' = Parser.parseModule filepath code
  let errors = Syntax.moduleErrors module'
  unless (null errors) $ do
    Text.IO.hPutStrLn stderr "Encountered parse errors while trying to format code:"
    forM_ errors $ \err -> do
      Text.IO.hPutStr stderr (toText err)
      Text.IO.hPutStr stderr "\n"
      exitFailure

  render <- getRender highlight
  render (Efg.Pretty.pretty module' <> Pretty.hardline)

  unless (filepath == "-") $ do
    withFile filepath WriteMode $ \handle -> do
      Efg.Pretty.renderIO False 80 handle (Efg.Pretty.pretty module')

main :: IO ()
main = do
  options <- Opts.execParser parserInfo

  case options of
    Interpret _file _highlight -> do
      Text.IO.hPutStr stderr "Interpreting not implemented yet\n"
      exitFailure
    Parse file highlight -> parse_ file highlight
    Format file highlight -> formatFile file highlight
