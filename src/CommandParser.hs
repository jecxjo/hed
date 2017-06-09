{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module CommandParser
  ( parseRange
  , parseCommand
  , ParsedCommand(..)
  , doParseCommand
  ) where

import Utils

import Data.Text (pack, singleton, replace, Text(..), concatMap)
import Text.Read (readMaybe)
import Text.Parsec ( (<|>) )
import Text.Parsec.Combinator ( many1
                              , choice
                              , optionMaybe
                              , optional
                              , eof
                              )
import Text.Parsec.Char ( char
                        , oneOf
                        , letter
                        , string
                        , digit
                        , noneOf
                        , endOfLine
                        , anyChar
                        )
import Text.Parsec.Error as E
import Text.Parsec.Text
import Text.Parsec.Prim ( parse
                        , try
                        , unexpected
                        , many
                        )

import Commands

type ParseError = E.ParseError

data ParsedCommand = ParsedCommand
  { cmdName :: CommandName
  , optionalRange :: Range
  , argsList :: Args
  }
  deriving (Show, Eq)

-- |
-- | Misc
-- |
orCurrent = justOr (SingleRange CurrentPosition)

-- |Parse and toss whitespace
whiteSpace :: Parser ()
whiteSpace = () <$ many (char ' ')

-- |Regular Expressions
forwardRegEx :: Parser Text
forwardRegEx = do
    _ <- char '/'
    re <- many $ chars
    _ <- char '/'
    return $ pack re
  where
    chars = escaped <|> noneOf "/\n"
    escaped = try (char '\\' >> choice (zipWith escapedChar codes replacements))
    escapedChar code replacement = char code >> return replacement
    codes =        ['/', '\"']
    replacements = ['/', '\"']

backwardRegEx :: Parser Text
backwardRegEx = do
    _ <- char '?'
    re <- many $ chars
    _ <- char '?'
    return $ pack re
  where
    chars = escaped <|> noneOf "?\n"
    escaped = try (char '\\' >> choice (zipWith escapedChar codes replacements))
    escapedChar code replacement = char code >> return replacement
    codes =        ['?', '\"']
    replacements = ['?', '\"']

-- |
-- | Line Addressing
-- |

-- |currentPosition
currentPosition :: Parser Position
currentPosition = do
  _ <- char '.'
  return CurrentPosition

-- |endOfDoc: $
endOfDoc :: Parser Position
endOfDoc = do
  _ <- char '$'
  return EndOfDoc

-- |absolutePosition
absolutePosition :: Parser Position
absolutePosition = do
  pos <- many1 digit
  return $ AbsolutePosition (read pos :: Int)

-- |relativePosition
relativePosition :: Parser Position
relativePosition = do
    dir <- oneOf "+-"
    pos <- many digit
    let pos' = getPosition (readMaybe pos :: Maybe Int)
    return $ case dir of
              '-' -> RelativePosition Minus pos'
              '+' -> RelativePosition Plus pos'
  where
    getPosition :: Maybe Int -> Int
    getPosition (Just v) = v
    getPosition Nothing = 1

-- |searchPosition: /foo/ ?bar?
searchPosition :: Parser Position
searchPosition = choice [next, previous]
  where
    next = do
      re <- forwardRegEx
      return $ NextLineMatching re
    previous = do
      re <- backwardRegEx
      return $ PreviousLineMatching re

-- |markLocation: 'x
markLocation :: Parser Position
markLocation = do
  _ <- char '\''
  pos <- letter
  return $ MarkLocation pos


-- |allRanges: . 1,$ -4,+2 ' ;
allRanges :: Parser Range
allRanges = choice [ try doubleRange, singleRange, specialDoublePosition ]

parseRange :: Parser Range
parseRange = allRanges

-- |specialDoublePosition is for single character multiposition ranges
-- ,{action} -> 1,$
-- %{action} -> 1,$
-- ;{action} -> .,$
specialDoublePosition :: Parser Range
specialDoublePosition = do
  _ <- whiteSpace
  position <- choice [ char ',', char '%', char ';' ]
  _ <- whiteSpace
  case position of
    ',' -> return $ DoubleRange (AbsolutePosition 1) EndOfDoc
    '%' -> return $ DoubleRange (AbsolutePosition 1) EndOfDoc
    ';' -> return $ DoubleRange CurrentPosition EndOfDoc
    _ -> unexpected "Parser Failure"

-- |doubleRange covers ranges with a comma
-- 1,2{action} -> Absolute
-- -1,+2{action} -> Relative
doubleRange :: Parser Range
doubleRange = do
  _ <- whiteSpace
  left <- choice [ currentPosition
                 , endOfDoc
                 , markLocation
                 , absolutePosition
                 , relativePosition
                 ]
  _ <- whiteSpace
  _ <- char ','
  _ <- whiteSpace
  right <- choice [ currentPosition
                  , endOfDoc
                  , markLocation
                  , absolutePosition
                  , relativePosition
                  ]
  _ <- whiteSpace
  return $ DoubleRange left right

-- |singleRange covers ranges with a single value
-- .{command} -> CurrentPosition
-- ${command} -> EndOfDoc
-- 1{command} -> AbsolutePosition
-- -2{command} -> RelativePosition
-- 'x{command} -> MarkLocation
-- /{search}/{command} -> NextLineMatching
-- ?{search}?{command} -> PreviousLineMatching
singleRange :: Parser Range
singleRange = do
  _ <- whiteSpace
  pos <- choice [ currentPosition
                , endOfDoc
                , markLocation
                , searchPosition
                , absolutePosition
                , relativePosition
                ]
  _ <- whiteSpace
  return $ SingleRange pos

-- |
-- | Commands
-- |

-- |append: (.)a
-- Appends text to the buffer after the addressed line
-- Range: None, Single
-- Args: None
appendCommand :: Parser ParsedCommand
appendCommand = do
  mRange <-  optionMaybe singleRange
  let r = orCurrent mRange
  _ <- whiteSpace
  _ <- char 'a'
  _ <- whiteSpace
  _ <- eof
  return $ ParsedCommand "append" r NoArgs

-- |change: (.,.)c
-- Changes lines in buffer, delete and and insert
-- Range: None, Single, Double
-- Args: None
changeCommand :: Parser ParsedCommand
changeCommand = do
  mRange <- optionMaybe allRanges
  let r = orCurrent mRange
  _ <- whiteSpace
  _ <- char 'c'
  _ <- whiteSpace
  _ <- eof
  return $ ParsedCommand "change" r NoArgs

-- |delete: (.,.)d
-- Delets addressed lines
-- Range: None, Single, Double
-- Args: None
deleteCommand :: Parser ParsedCommand
deleteCommand = do
  mRange <-  optionMaybe allRanges
  let r = orCurrent mRange
  _ <- whiteSpace
  _ <- char 'd'
  _ <- whiteSpace
  _ <- eof
  return $ ParsedCommand "delete" r NoArgs

-- |edit: e <filename>
-- Edits the file. Warning printed if changes haven't been saved
-- Range: None
-- Args: filename
editCommand :: Parser ParsedCommand
editCommand = choice [try withFileName, try withoutFileName]
  where
    withFileName = do
      _ <- whiteSpace
      _ <- char 'e'
      _ <- many1 (char ' ')
      filename <- many1 $ noneOf "\n"
      _ <- eof
      (return . ParsedCommand "edit" NoRange) . StringArg $ pack filename
    withoutFileName = do
      _ <- whiteSpace
      _ <- char 'e'
      _ <- whiteSpace
      _ <- eof
      (return . ParsedCommand "edit" NoRange) . StringArg $ pack ""

-- |edit unconditional: E <filename>
-- Same as 'e' but does not warn if unsaved
-- Range: none
-- Args: filename
editUncondCommand :: Parser ParsedCommand
editUncondCommand = do
  _ <- whiteSpace
  _ <- char 'E'
  _ <- many1 (char ' ')
  filename <- many1 $ noneOf "\n"
  _ <- eof
  (return . ParsedCommand "editUncond" NoRange) . StringArg $ pack filename

-- |default filename: f <filename>
-- sets the default file name
-- Range: none
-- Args: filename
defaultFileCommand :: Parser ParsedCommand
defaultFileCommand = choice [try withFileName, try withoutFileName]
  where
    withFileName = do
      _ <- whiteSpace
      _ <- char 'f'
      _ <- many1 (char ' ')
      filename <- many1 $ noneOf "\n "
      _ <- whiteSpace
      _ <- eof
      (return . ParsedCommand "defaultFile" NoRange) . StringArg $ pack filename
    withoutFileName = do
      _ <- whiteSpace
      _ <- char 'f'
      _ <- whiteSpace
      _ <- eof
      return $ ParsedCommand "defaultFile" NoRange  NoArgs

-- |(1,$)g/re/cmd
-- global command
-- Range: N,S,M
-- Args: RegEx
globalCommand :: Parser ParsedCommand
globalCommand = do
  _ <- whiteSpace
  mRange <- optionMaybe allRanges
  let r = justOr (DoubleRange (AbsolutePosition 1) EndOfDoc) mRange
  _ <- whiteSpace
  _ <- char 'g'
  re <- forwardRegEx
  cmd <- many1 $ noneOf "\n"
  _ <- eof
  return . ParsedCommand "global" r $ RegExArg re (pack cmd)

-- |(1,$)G/re/ -- Interactive
-- global command, interactive
-- Range: N,S,M
-- Args: Half a RegEx
globalInteractiveCommand :: Parser ParsedCommand
globalInteractiveCommand = do
  _ <- whiteSpace
  mRange <- optionMaybe allRanges
  let r = justOr (DoubleRange (AbsolutePosition 1) EndOfDoc) mRange
  _ <- whiteSpace
  _ <- char 'G'
  re <- forwardRegEx
  _ <- whiteSpace
  _ <- eof
  return . ParsedCommand "globalInteractive" r $ RegExArg re ""

-- |help: h
-- explain last error
-- Range: None
-- Args: None
helpCommand :: Parser ParsedCommand
helpCommand = do
  _ <- whiteSpace
  _ <- char 'h'
  _ <- whiteSpace
  _ <- eof
  return $ ParsedCommand "help" NoRange NoArgs

-- |toggle help: H
-- toggle displaying help
-- Range: None
-- Args: None
toggleHelpCommand :: Parser ParsedCommand
toggleHelpCommand = do
  _ <- whiteSpace
  _ <- char 'H'
  _ <- whiteSpace
  _ <- eof
  return $ ParsedCommand "toggleHelp" NoRange NoArgs

-- |(.)i
-- Insert at position
-- Range: None, Single
-- Args: None
insertCommand :: Parser ParsedCommand
insertCommand = do
  mRange <- optionMaybe singleRange
  let r = orCurrent mRange
  _ <- whiteSpace
  _ <- char 'i'
  _ <- whiteSpace
  _ <- eof
  return $ ParsedCommand "insert" r NoArgs

-- |(.,.+1)j
-- Join multiple lines
-- Range: None, Multi
-- Args: None
joinCommand :: Parser ParsedCommand
joinCommand = do
  mRange <- optionMaybe doubleRange
  let r = justOr (DoubleRange CurrentPosition (RelativePosition Plus 1)) mRange
  _ <- whiteSpace
  _ <- char 'j'
  _ <- whiteSpace
  _ <- eof
  return $ ParsedCommand "join" r NoArgs

-- |(.)kx
-- Mark a location
-- Range: None, Single
-- Args: label
markCommand :: Parser ParsedCommand
markCommand = do
  mRange <- optionMaybe singleRange
  let r = orCurrent mRange
  _ <- whiteSpace
  _ <- char 'k'
  lbl <- letter
  _ <- whiteSpace
  _ <- eof
  (return . ParsedCommand "mark" r) . StringArg $ singleton lbl

-- |(.,.)l
-- List lines, ends with $
-- Range: None, Single, Multi
-- Args: None
listCommand :: Parser ParsedCommand
listCommand = do
  mRange <- optionMaybe allRanges
  let r = orCurrent mRange
  _ <- whiteSpace
  _ <- char 'l'
  _ <- whiteSpace
  _ <- eof
  return $ ParsedCommand "list" r NoArgs

-- |(.,.)m(.) -- Move
-- Move
-- Range: N,S,M
-- Args: N,S Range
moveCommand :: Parser ParsedCommand
moveCommand = do
  _ <- whiteSpace
  mRange <- optionMaybe allRanges
  let r = orCurrent mRange
  _ <- whiteSpace
  _ <- char 'm'
  _ <- whiteSpace
  mRange' <- optionMaybe singleRange
  let r' = orCurrent mRange'
  _ <- whiteSpace
  _ <- eof
  return . ParsedCommand "move" r $ RangeArg r'


-- |(.,.)n
-- Print numbered
-- Range: None, Single, Multi
-- Args: None
numberedCommand :: Parser ParsedCommand
numberedCommand = do
  mRange <- optionMaybe allRanges
  let r = orCurrent mRange
  _ <- whiteSpace
  _ <- char 'n'
  _ <- whiteSpace
  _ <- eof
  return $ ParsedCommand "numbered" r NoArgs

-- |(.,.)p -- Print lines
printCommand :: Parser ParsedCommand
printCommand = do
  mRange <- optionMaybe allRanges
  let r = orCurrent mRange
  _ <- whiteSpace
  _ <- char 'p'
  _ <- whiteSpace
  _ <- eof
  return $ ParsedCommand "print" r NoArgs

-- |P
-- toggle prompt
-- Range: None
-- Args: None
togglePromptCommand :: Parser ParsedCommand
togglePromptCommand = do
  _ <- whiteSpace
  _ <- char 'P'
  _ <- whiteSpace
  _ <- eof
  return $ ParsedCommand "togglePrompt" NoRange NoArgs

-- |quit: q
-- Range: None
-- Args: None
quitCommand :: Parser ParsedCommand
quitCommand = do
  _ <- whiteSpace
  _ <- char 'q'
  _ <- whiteSpace
  _ <- eof
  return $ ParsedCommand "quit" NoRange NoArgs

-- |quit unconditional: Q
-- Range: None
-- Args: None
quitUncondCommand :: Parser ParsedCommand
quitUncondCommand = do
  _ <- whiteSpace
  _ <- char 'Q'
  _ <- whiteSpace
  _ <- eof
  return $ ParsedCommand "quitUncond" NoRange NoArgs

-- |read file: ($)r <file>
-- Range: N,S
-- Args: Filename
readCommand :: Parser ParsedCommand
readCommand = do
  _ <- whiteSpace
  mRange <- optionMaybe singleRange
  let r = justOr (SingleRange EndOfDoc) mRange
  _ <- whiteSpace
  _ <- char 'r'
  _ <- many1 (char ' ') -- Must have at least one space
  filename <- many1 $ noneOf "\n"
  _ <- eof
  (return . ParsedCommand "read" r) . StringArg $ pack filename

-- |(.,.)s/re/replace
-- Range: N,S,M
-- Args: RegEx
swapCommand :: Parser ParsedCommand
swapCommand = do
  _ <- whiteSpace
  mRange <- optionMaybe allRanges
  let r = orCurrent mRange
  _ <- whiteSpace
  _ <- char 's'
  re <- forwardRegEx
  replace <- many1 $ noneOf "/\n"
  _ <- optional $ char '/'
  _ <- eof
  return . ParsedCommand "swap" r $ RegExArg re (pack replace)


-- |(.,.)s -- replace last match
-- Range: None
-- Args: None
swapLastCommand :: Parser ParsedCommand
swapLastCommand = do
  _ <- whiteSpace
  mRange <- optionMaybe allRanges
  let r = orCurrent mRange
  _ <- whiteSpace
  _ <- char 's'
  _ <- whiteSpace
  _ <- eof
  return $ ParsedCommand "swapLast" r NoArgs

-- |(.,.)t(.) -- copies
-- Range: NSM
-- Args: Range NS
copyCommand :: Parser ParsedCommand
copyCommand = do
  _ <- whiteSpace
  mRange <- optionMaybe allRanges
  let r = orCurrent mRange
  _ <- whiteSpace
  _ <- char 't'
  _ <- whiteSpace
  mRange' <- optionMaybe singleRange
  let r' = orCurrent mRange'
  _ <- whiteSpace
  _ <- eof
  return . ParsedCommand "copy" r $ RangeArg r'

-- |undo last command: u
-- Range: None
-- Args: None
undoCommand :: Parser ParsedCommand
undoCommand = do
  _ <- whiteSpace
  _ <- char 'u'
  _ <- whiteSpace
  _ <- eof
  return $ ParsedCommand "undo" NoRange NoArgs

-- |(1,$)v/re/cmd
-- same as g only for non-match
-- Range: None
-- Args: None
reverseGlobalCommand :: Parser ParsedCommand
reverseGlobalCommand = do
  _ <- whiteSpace
  mRange <- optionMaybe allRanges
  let r = justOr (DoubleRange (AbsolutePosition 1) EndOfDoc) mRange
  _ <- whiteSpace
  _ <- char 'v'
  re <- forwardRegEx
  cmd <- many1 $ noneOf "\n"
  _ <- eof
  return . ParsedCommand "reverseGlobal" r $ RegExArg re (pack cmd)

-- |(1,$)V/re/
-- same as G only for non-match
-- Range: None
-- Args: None
reverseGlobalInteractiveCommand :: Parser ParsedCommand
reverseGlobalInteractiveCommand = do
  _ <- whiteSpace
  mRange <- optionMaybe allRanges
  let r = justOr (DoubleRange (AbsolutePosition 1) EndOfDoc) mRange
  _ <- whiteSpace
  _ <- char 'V'
  re <- forwardRegEx
  _ <- whiteSpace
  _ <- eof
  return . ParsedCommand "reverseGlobalInteractive" r $ RegExArg re ""

-- |(1,$)w <file>
-- write addresses to file
-- Range: N,S,M
-- Args: Filename
writeCommand :: Parser ParsedCommand
writeCommand = choice [try withFileName , try withoutFileName]
  where
    withFileName = do
      _ <- whiteSpace
      mRange <- optionMaybe allRanges
      let r = justOr (DoubleRange (AbsolutePosition 1) EndOfDoc) mRange
      _ <- whiteSpace
      _ <- char 'w'
      _ <- many1 $ char ' '
      filename <-  many1 $ noneOf "\n"
      _ <- eof
      (return . ParsedCommand "write" r) . StringArg $ pack filename
    withoutFileName = do
      _ <- whiteSpace
      mRange <- optionMaybe allRanges
      let r = justOr (DoubleRange (AbsolutePosition 1) EndOfDoc) mRange
      _ <- whiteSpace
      _ <- char 'w'
      _ <- whiteSpace
      _ <- eof
      (return . ParsedCommand "write" r) . StringArg $ pack ""

-- |(1,$)wq <file> -- write addresses to file and quit
-- Range: None
-- Args: None
writeQuitCommand :: Parser ParsedCommand
writeQuitCommand = choice [try withFileName, try withoutFileName]
  where
    withFileName = do
      _ <- whiteSpace
      mRange <- optionMaybe allRanges
      let r = justOr (DoubleRange (AbsolutePosition 1) EndOfDoc) mRange
      _ <- whiteSpace
      _ <- string "wq"
      _ <- many1 $ char ' '
      filename <-  many1 $ noneOf "\n"
      _ <- eof
      (return . ParsedCommand "writeQuit" r) . StringArg $ pack filename
    withoutFileName = do
      _ <- whiteSpace
      mRange <- optionMaybe allRanges
      let r = justOr (DoubleRange (AbsolutePosition 1) EndOfDoc) mRange
      _ <- whiteSpace
      _ <- string "wq"
      _ <- whiteSpace
      _ <- eof
      (return . ParsedCommand "writeQuit" r) . StringArg $ pack ""

-- |(1,$)W <file> -- write addresses to end of file
-- Range: NSM
-- Args: Filename
appendFileCommand :: Parser ParsedCommand
appendFileCommand = do
  _ <- whiteSpace
  mRange <- optionMaybe allRanges
  let r = justOr (DoubleRange (AbsolutePosition 1) EndOfDoc) mRange
  _ <- whiteSpace
  _ <- char 'W'
  _ <- many1 (char ' ')
  filename <- many $ noneOf "\n"
  _ <- eof
  (return . ParsedCommand "appendFile" r) . StringArg $ pack filename

-- |(.)x -- Copies (puts) contents to address
-- Range: NS
-- Args: None
putsCommand :: Parser ParsedCommand
putsCommand = do
  mRange <- optionMaybe singleRange
  let r = orCurrent mRange
  _ <- whiteSpace
  _ <- char 'x'
  _ <- whiteSpace
  _ <- eof
  return $ ParsedCommand "puts" r NoArgs

-- |(.,.)y -- Copies (yank) contents from address
-- Range: None
-- Args: None
yankCommand :: Parser ParsedCommand
yankCommand = do
  _ <- whiteSpace
  mRange <- optionMaybe allRanges
  let r = orCurrent mRange
  _ <- whiteSpace
  _ <- char 'y'
  _ <- whiteSpace
  _ <- eof
  return $ ParsedCommand "yank" r NoArgs

-- |(.+1)zn
-- scrolls n lines at a time at address line
-- Range: NS
-- Args: Count
scrollCommand :: Parser ParsedCommand
scrollCommand = do
  _ <- whiteSpace
  mRange <- optionMaybe singleRange
  let r = justOr (SingleRange $ RelativePosition Plus 1) mRange
  _ <- whiteSpace
  _ <- char 'z'
  count <- many digit
  _ <- whiteSpace
  _ <- eof
  if null count
  then return . ParsedCommand "scroll" r $ NoArgs
  else return . ParsedCommand "scroll" r $ NumberArg (read count :: Int)

-- |scrolls n lines, numbered, at a time at address line
-- Range: NS
-- Args: Count
scrollNumberedCommand :: Parser ParsedCommand
scrollNumberedCommand = do
  _ <- whiteSpace
  mRange <- optionMaybe singleRange
  let r = justOr (SingleRange $ RelativePosition Plus 1) mRange
  _ <- whiteSpace
  _ <- char 'Z'
  count <- many digit
  _ <- whiteSpace
  _ <- eof
  if null count
  then return . ParsedCommand "scrollNumbered" r $ NoArgs
  else return . ParsedCommand "scrollNumbered" r $ NumberArg (read count :: Int)

-- !command
--
bangCommand :: Parser ParsedCommand
bangCommand = do
  _ <- whiteSpace
  _ <- char '!'
  cmd <- many1 $ noneOf "\n"
  _ <- eof
  (return . ParsedCommand "bang" NoRange) . StringArg $ pack cmd

-- |(.,.)#
commentCommand :: Parser ParsedCommand
commentCommand = do
  _ <- whiteSpace
  mRange <- optionMaybe allRanges
  let r = orCurrent mRange
  _ <- whiteSpace
  _ <- char '#'
  comment <- many $ noneOf "\n"
  _ <- eof
  (return . ParsedCommand "comment" r) . StringArg $ pack comment

-- |($)=
lineNumberCommand :: Parser ParsedCommand
lineNumberCommand = do
  _ <- whiteSpace
  mRange <- optionMaybe singleRange
  let r = justOr (SingleRange EndOfDoc) mRange
  _ <- whiteSpace
  _ <- char '='
  _ <- whiteSpace
  _ <- eof
  return $ ParsedCommand "lineNumber" r NoArgs

-- |(.+1)<newline>
jumpCommand :: Parser ParsedCommand
jumpCommand = do
  _ <- whiteSpace
  mRange <- optionMaybe singleRange
  let r = justOr NoRange mRange
  _ <- whiteSpace
  _ <- eof
  return $ ParsedCommand "jump" r NoArgs

-- |Command List
-- Example: ?
commandListCommand :: Parser ParsedCommand
commandListCommand = do
  _ <- whiteSpace
  char '?'
  _ <- whiteSpace
  term <- many $ noneOf "\n"
  _ <- eof
  if null term
  then return $ ParsedCommand "commandList" NoRange NoArgs
  else return $ ParsedCommand "commandList" NoRange (StringArg $ pack term)

-- |parse commands
parseCommand :: Parser ParsedCommand
parseCommand = choice [ try appendCommand
                      , try changeCommand
                      , try deleteCommand
                      , try editCommand
                      , try editUncondCommand
                      , try defaultFileCommand
                      , try globalCommand
                      , try globalInteractiveCommand
                      , try helpCommand
                      , try toggleHelpCommand
                      , try insertCommand
                      , try joinCommand
                      , try markCommand
                      , try listCommand
                      , try moveCommand
                      , try numberedCommand
                      , try printCommand
                      , try togglePromptCommand
                      , try quitCommand
                      , try quitUncondCommand
                      , try readCommand
                      , try swapCommand
                      , try swapLastCommand
                      , try copyCommand
                      , try undoCommand
                      , try reverseGlobalCommand
                      , try reverseGlobalInteractiveCommand
                      , try writeQuitCommand
                      , try writeCommand
                      , try appendFileCommand
                      , try putsCommand
                      , try yankCommand
                      , try bangCommand
                      , try scrollCommand
                      , try scrollNumberedCommand
                      , try commentCommand
                      , try lineNumberCommand
                      , try jumpCommand
                      , try commandListCommand
                      ]

doParseCommand = parse parseCommand ""
