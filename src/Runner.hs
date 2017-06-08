{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Runner (runEditor) where

import Commands
import CommandParser
import RegEx
import Resources

import Control.Applicative ((<$>))
import Control.Exception (catch, IOException(..))
import Control.Monad
import Control.Monad.State
import qualified Data.ByteString.Char8 as Char8
import Data.List (intercalate, find)
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import Data.Text.IO (readFile, writeFile)
import qualified Data.Vector as V
import Prelude hiding (readFile, writeFile)
import System.Directory (doesFileExist)
import System.IO (hFlush, stdout)
import System.IO (stderr, hPutStr)
import System.Process (callCommand, readCreateProcess, shell)
import Text.Regex.PCRE

-- |Initial state
initialState :: StateInfo
initialState = StateInfo
                  V.empty -- contents
                  ""  -- fileName
                  False -- dirty
                  False -- dirtyWarning
                  0 -- position
                  ":" -- prompt
                  True -- showPrompt
                  "" -- lastError
                  V.empty -- clipboard
                  False -- displayHelp
                  V.empty -- marks
                  "" -- lastSearch
                  "" -- lastReplace
                  22 -- lastScroll (default is 22)
                  -- Temporary Data
                  V.empty -- tBuffer
                  (-1) -- tFromStart
                  (-1) -- tFromEnd
                  (-1) -- tToStart
                  (-1) -- tToEnd

data Command = Command
  { name :: CommandName
  , doWith :: Range -> Args -> Invocation
  }

data Invocation = Invocation
  { ofCommand :: Command
  , range :: Range
  , args :: Args
  , exec :: CommandState
  }

-- |Command Mode handles all commands
commandMode :: CommandState
commandMode = do
  st <- get
  ((when (showPrompt st) . lift) . putStr) . T.unpack $ prompt st
  lift $ hFlush stdout
  cmd <- lift getLine
  let parsedCmd = doParseCommand (T.pack  cmd)
  case parsedCmd of
    Right (ParsedCommand cmdString r a) ->
      case parseActions actions cmdString r a of
        Just i -> exec i
        Nothing -> do
          let newSt = st { lastError = T.append "Unknown Command: " cmdString }
          printError newSt
          put newSt
          commandMode
    Left e -> do
      let newSt = st { lastError = T.append "Unknown Command: " (T.pack $ show e) }
      printError newSt
      put newSt
      commandMode

-- |Insert mode gathers lines and writes them to state
insertMode :: CommandState
insertMode = do
  st <- get
  newLine <- lift getLine
  case newLine of
    "." -> do
        let results = insertTextM st
        case results of
          Left st' -> do
            printError st'
            put (clearTemps st')
          Right st' ->
            put (clearTemps st' { position = position st' + V.length (tBuffer st')
                                , dirty = True
                                , dirtyWarning = False
                                })
        commandMode
    x -> do
      put st { tBuffer = V.snoc (tBuffer st) (T.pack x) }
      insertMode

-- |Parses actions looking for one with a matching command name
parseActions :: V.Vector Command -> T.Text -> Range -> Args -> Maybe Invocation
parseActions legalCommands cmdName r a =
  (\c -> doWith c r a) <$>
    V.find (\cmd -> name cmd == cmdName) legalCommands

-- |Actions
actions :: V.Vector Command
actions = V.fromList [ appendAction
                     , changeAction
                     , deleteAction
                     , editAction
                     , editUnconditionalAction
                     , defaultFileAction
                     -- , globalAction
                     -- , globalInteractiveAction
                     , helpAction
                     , toggleHelpAction
                     , insertAction
                     , joinAction
                     , markAction
                     , listAction
                     , moveAction
                     , numberedAction
                     , printAction
                     , togglePromptAction
                     , quitAction
                     , quitUnconditionalAction
                     , readAction
                     , swapAction
                     , swapLastAction
                     , copyAction
                     -- , undoAction
                     -- , reverseGlobalAction
                     -- , reverseGlobalInteractiveAction
                     , writeAction
                     , writeQuitAction
                     , appendFileAction
                     , putsAction
                     , yankAction
                     , scrollAction
                     , bangAction
                     , commentAction
                     , lineNumberAction
                     , jumpAction
                     , commandListAction
                     ]

-- |appendAction inserts text after the addressed line
appendAction =
  Command "append" $ \r a ->
    Invocation appendAction r a $ do
      st <- get
      let results = setToRangeM (Just r) st
                      >>= setPositionToToM
      case results of
        Left st' -> do
          printError st'
          put (clearTemps st')
          commandMode
        Right st' -> do
          put st'
          insertMode

-- |changeAction delets the addressed lines and then inserts 
changeAction =
  Command "change" $ \r a ->
    Invocation changeAction r a $ do
      st <- get
      let results = setFromRangeM r st
                        >>= setToRangeM (Just r)
                        >>= fixToRangeM
                        >>= removeContentsM
                        >>= setPositionToToM
      case results of
        Left st' -> do
          printError st'
          put (clearTemps st')
          commandMode
        Right st' -> do
          put st'
          insertMode

-- |deleteAction deletes addressed lines
deleteAction =
  Command "delete" $ \r a ->
    Invocation deleteAction r a $ do
      st <- get
      let results = setFromRangeM r st
                      >>= removeContentsM
                      >>= fixFromRangeM
                      >>= setPositionToFromM
      case results of
        Left st' -> do
          printError st'
          put (clearTemps st')
        Right st' -> put (clearTemps st')
      commandMode

-- |editAction opens a file and replaces the current buffer
editAction =
  Command "edit" $ \r a ->
      Invocation editAction r a $ do
        st <- get
        if dirty st && not (dirtyWarning st)
        then do
          let newSt = st { lastError = "Warning: buffer modified"
                         , dirtyWarning = True
                         }
          put newSt
          let err = if displayHelp newSt
                    then  (T.unpack . lastError $ newSt) ++ "\n?"
                    else "?"
          lift $ putStrLn err
          lift $ hFlush stdout
        else do
          let StringArg path = a
          let path' = case (T.unpack path, fileName st) of
                        ("", "") -> ""
                        ("", x) -> x
                        (x, _ ) -> x
          if null path'
          then do
            let newSt = st { lastError = "No file name" }
            put newSt
            let err = if displayHelp newSt
                      then  (T.unpack . lastError $ newSt) ++ "\n?"
                      else "?"
            lift $ putStrLn err
            lift $ hFlush stdout
          else do
            exists <- lift $ doesFileExist path'
            if exists
            then do
              lns <- lift $ readFile path'
              let textLines = V.fromList $ T.lines lns
              let newSt = st { contents = textLines
                            , fileName = path'
                            , dirty = False
                            , dirtyWarning = False
                            , position = 0
                            , lastError = T.empty
                            }
              put newSt
              (lift . print) $ T.length lns
            else do
              let newSt = st { lastError = "Cannot open input file" }
              put newSt
              let err = if displayHelp newSt
                        then  (T.unpack . lastError $ newSt) ++ "\n?"
                        else "?"
              lift $ putStrLn err
              lift $ hFlush stdout
        commandMode

-- |editUnconditionalAction edits a file, but without a check if the buffer is dirty
editUnconditionalAction =
  Command "editUncond" $ \r a ->
    Invocation editUnconditionalAction r a $ do
      st <- get
      let StringArg path = a
      let path' = T.unpack path
      exists <- lift $ doesFileExist path'
      if exists
      then do
        lns <- lift $ readFile path'
        let textLines = V.fromList $ T.lines lns
        let newSt = st { contents = textLines
                       , fileName = path'
                       , dirty = False
                       , dirtyWarning = False
                       , position = 0
                       , lastError = T.empty
                       }
        put newSt
        (lift . print) $ T.length lns
      else do
        let newSt = st { lastError = "Cannot open input file" }
        put newSt
        let err = if displayHelp newSt
                  then  (T.unpack . lastError $ newSt) ++ "\n?"
                  else "?"
        lift $ putStrLn err
        lift $ hFlush stdout
      commandMode

-- |defaultFileAction set the default file name or prints current if none supplied
defaultFileAction =
  Command "defaultFile" $ \r a ->
    Invocation defaultFileAction r a $ do
      st <- get
      case a of
        StringArg name -> do
          let newSt = st { fileName = T.unpack name
                         , lastError = T.append (T.pack "Default File Name: ") name }
          put newSt
        _ -> if not (null (fileName st))
             then lift . putStrLn $ fileName st
             else do
                let newSt = st { lastError = "No current filename" }
                put newSt
                let err = if displayHelp newSt
                          then  (T.unpack . lastError $ newSt) ++ "\n?"
                          else "?"
                lift $ putStrLn  err
                lift $ hFlush stdout
      commandMode

-- |globalAction
-- |globalInteractiveAction

-- |helpAction displays the last error
helpAction =
  Command "help" $ \r a ->
    Invocation helpAction r a $ do
      st <- get
      lift $ (putStrLn . T.unpack) (lastError st)
      lift $ print st
      lift $ hFlush stdout
      commandMode

-- |toggleHelpAction toggles if last error is printed automatically
toggleHelpAction =
  Command "toggleHelp" $ \r a ->
    Invocation toggleHelpAction r a $ do
      st <- get
      let newSt = st { displayHelp = not (displayHelp st) }
      put newSt
      commandMode

-- |insertAction enters Insert Mode before the addressed line
insertAction =
  Command "insert" $ \r a ->
    Invocation insertAction r a $ do
      st <- get
      let results = setToRangeM (Just r) st
                      >>= fixToRangeM
                      >>= setPositionToToM
      case results of
        Left st' -> do
          printError st'
          put (clearTemps st')
          commandMode
        Right st' -> do
          put st'
          insertMode

-- |joinAction joines the text on addressed lines to a single line
joinAction =
  Command "join" $ \r a ->
    Invocation joinAction r a $ do
      st <- get
      let results = setFromRangeM r st
                      >>= sliceContentsM
                      >>= setToRangeM (Just r)
                      >>= fixJoinRange
                      >>= removeContentsM
                      >>= joinBuffer
                      >>= insertTextM
      case results of
        Left st' -> do
          printError st'
          put (clearTemps st')
        Right st' -> put (clearTemps st')
      commandMode
    where
      fixJoinRange st =
        Right st { tToStart = tToStart st - 1 } -- Insert rather than append
      joinBuffer st =
        Right st { tBuffer =
          V.singleton . V.foldr T.append T.empty $ tBuffer st }

-- |markAction sets a mark on the addressed line
markAction =
  Command "mark" $ \r a ->
    Invocation markAction r a $ do
        st <- get
        let results = setFromRangeM r st
                        >>= removeMark (getStringArgument a)
                        >>= addMark (getStringArgument a)
        case results of
          Left st' -> do
            printError st'
            put (clearTemps st')
          Right st' -> put (clearTemps st')
        commandMode
      where
        removeMark Nothing st = Left st { lastError = "Invalid address" }
        removeMark (Just s) st =
          Right st { marks = V.filter (\(c',_) -> c' /= c) (marks st) }
          where c = head $ T.unpack s
        addMark Nothing st = Left st { lastError = "Invalid address" }
        addMark (Just s) st =
          Right st { marks = V.cons (c, tFromStart st) (marks st) }
          where c = head $ T.unpack s

-- |listAction
listAction =
  Command "list" $ \r a ->
    Invocation listAction r a $ do
        st <- get
        let results = setFromRangeM r st
                        >>= sliceContentsM
                        >>= convertLines
        case results of
          Left st' -> do
            printError st'
            put (clearTemps st')
          Right st' -> do
            lift $ printBuff st'
            put (clearTemps st')
        commandMode
      where
        convertLines st = Right st { tBuffer = V.map (\l -> T.append (convert l) "$") (tBuffer st) }
        convert txt = T.concatMap fn txt
        codes =   ['\NUL',  '\SOH',  '\STX',  '\ETX',  '\EOT',  '\ENQ',  '\ACK',  '\a',  '\b',  '\t',  '\n',  '\v',  '\f',  '\r',  '\SO',  '\SI',  '\DLE',  '\DC1',  '\DC2',  '\DC3',  '\DC4',  '\NAK',  '\SYN',  '\ETB',  '\CAN',  '\EM',  '\SUB',  '\ESC',  '\FS',  '\GS',  '\RS',  '\US',  '\DEL',  '\\',   '$']
        strings = ["\\NUL", "\\SOH", "\\STX", "\\ETX", "\\EOT", "\\ENQ", "\\ACK", "\\a", "\\b", "\\t", "\\n", "\\v", "\\f", "\\r", "\\SO", "\\SI", "\\DLE", "\\DC1", "\\DC2", "\\DC3", "\\DC4", "\\NAK", "\\SYN", "\\ETB", "\\CAN", "\\EM", "\\SUB", "\\ESC", "\\FS", "\\GS", "\\RS", "\\US", "\\DEL", "\\\\", "\\$"]
        fn c = case find (\(x,_) -> x == c) (zip codes strings) of
                Just (_,y) -> T.pack y
                Nothing -> T.singleton c

-- |moveAction moves addressed lines to another location
moveAction =
  Command "move" $ \r a ->
    Invocation moveAction r a $ do
      st <- get
      let results = setFromRangeM r st
                      >>= setToRangeM (getRangeArgument a)
                      >>= shiftToByFromM
                      >>= sliceContentsM
                      >>= removeContentsM
                      >>= insertTextM
                      >>= setPositionToToM
      case results of
        Left st' -> do
          printError st'
          put (clearTemps st')
        Right st' -> put (clearTemps st')
      commandMode

-- |numberedAction prints addressed lines with numbers
numberedAction =
  Command "numbered" $ \r a ->
    Invocation numberedAction r a $ do
        st <-get
        let results = setFromRangeM r st
                        >>= sliceContentsM
                        >>= numberBuff
        case results of
          Left st' -> do
            printError st'
            put (clearTemps st')
          Right st -> do
            lift $ printBuff st
            put (clearTemps st)
        commandMode
      where
        numberBuff st =
          Right st { tBuffer =
            V.map (\(i,t) -> T.append
                              (T.pack (show (i + tFromStart st) ++ "\t"))
                              t)
                  (V.indexed (tBuffer st)) }

-- |printAction prints addressed lines
printAction =
  Command "print" $ \r a ->
    Invocation printAction r a $ do
      st <- get
      let results = setFromRangeM r st
                      >>= sliceContentsM
      case results of
        Left st' -> do
          printError st'
          put (clearTemps st')
        Right st -> do
          lift $ printBuff st
          put (clearTemps st)
      commandMode

-- |togglePromptAction toggles if prompt is shown when in Insert Mode
togglePromptAction =
  Command "togglePrompt" $ \r a ->
    Invocation togglePromptAction r a $ do
      st <- get
      put $ st { showPrompt = not (showPrompt st) }
      commandMode

-- |quitAction quits app unless dirty
quitAction =
  Command "quit" $ \r a ->
    Invocation quitAction r a $ do
      st <- get
      let result = checkDirtyM st
      case result of
        Left st' -> do
          printError st'
          put st'
          commandMode
        Right _ -> return ()

-- |quitUnconditionalAction quits app without checking dirty flag
quitUnconditionalAction =
  Command "quitUncond" $ \r a ->
    Invocation quitUnconditionalAction r a $ return ()

-- |readAction reads file and inserts contents at addressed location
readAction =
  Command "read" $ \r a ->
    Invocation readAction r a $ do
        st <- get
        let StringArg path = a
        if T.head path == '!'
        then do
          -- TODO Run shell and insert output into location
          let newSt = st { lastError = "read bang not supported at this time" }
          let err = if displayHelp newSt
                    then  (T.unpack . lastError $ newSt) ++ "\n?"
                    else "?"
          lift $ putStrLn err
          lift $ hFlush stdout
          put newSt
        else do
          -- Assumed passed a file
          let path' = T.unpack path
          exists <- lift $ doesFileExist path'
          if exists
          then do
            lns <- lift $ readFile path'
            let textLines = V.fromList $ T.lines lns
            case getPosition st (rangeToPos r) of
              -1 -> do
                let newSt = st { lastError = "Invalid address" }
                put newSt
                let err = if displayHelp newSt
                          then  (T.unpack . lastError $ newSt) ++ "\n?"
                          else "?"
                lift $ putStrLn  err
                lift $ hFlush stdout
              x -> do
                let newContent = insertText st x textLines
                let newSt = st { contents = newContent
                                , dirty = True
                                , dirtyWarning = False
                                }
                put newSt
          else do
            let newSt = st { lastError = "File Not Found" }
            put newSt
            let err = if displayHelp newSt
                      then  (T.unpack . lastError $ newSt) ++ "\n?"
                      else "?"
            lift $ putStrLn err
            lift $ hFlush stdout
        commandMode
      where
        rangeToPos (SingleRange p) = p
        rangeToPos _ = NoPosition

-- |swapAction
swapAction =
  Command  "swap" $ \r a ->
    Invocation swapAction r a $ do
        st <- get
        let results = setFromRangeM r st
                        >>= setToRangeM (Just r)
                        >>= fixToRangeM
                        >>= sliceContentsM
                        >>= swapContentsM (getRegExArgument a)
                        >>= removeContentsM
                        >>= insertTextM
                        >>= setPositionToToM
        case results of
          Left st' -> do
            printError st'
            put (clearTemps st')
          Right st' -> put (clearTemps st')
        commandMode
      where
        nextSearch fst snd = if T.null fst then snd else fst
        swapContentsM Nothing st = Left st { lastError = "Not a Regular Expression" }
        swapContentsM (Just (re,replace)) st = do
          let re' = nextSearch re (lastSearch st)
          let swap = swapGen (T.unpack re') (T.unpack replace)
          let updated = V.map (\ln -> (T.pack . swap) $ T.unpack ln) (tBuffer st)
          Right st { tBuffer = updated
                   , lastSearch = re'
                   , lastReplace = replace
                   }

-- |swapLastAction
swapLastAction =
  Command "swapLast" $ \r a ->
    Invocation swapLastAction r a $ do
        st <- get
        let results = setFromRangeM r st
                        >>= setToRangeM (Just r)
                        >>= fixToRangeM
                        >>= sliceContentsM
                        >>= swapContentsLastM
                        >>= removeContentsM
                        >>= insertTextM
                        >>= setPositionToToM
        case results of
          Left st' -> do
            printError st'
            put (clearTemps st')
          Right st' -> put (clearTemps st')
        commandMode
      where
        swapContentsLastM st = do
          let swap = swapGen (T.unpack $ lastSearch st) (T.unpack $ lastReplace st)
          let updated = V.map (\ln -> (T.pack . swap) $ T.unpack ln) (tBuffer st)
          Right st { tBuffer = updated }

-- |copyAction copies from addressed location to destination
copyAction =
  Command "copy" $ \r a ->
    Invocation copyAction r a $ do
      st <- get
      let results = setFromRangeM r st
                      >>= setToRangeM (getRangeArgument a)
                      >>= sliceContentsM
                      >>= insertTextM
                      >>= setPositionToToM
      case results of
        Left st' -> do
          printError st'
          put (clearTemps st')
        Right st' -> put (clearTemps st')
      commandMode

-- |undoAction
-- |reverseGlobalAction
-- |reverseGlobalInteractiveAction

-- |writeAction writes addressed range to file
writeAction =
  Command "write" $ \r a ->
    Invocation writeAction r a $ do
      st <- get
      let StringArg path = a
      let path' = case (T.unpack path, fileName st) of
                    ("", "") -> ""
                    ("", x) -> x
                    (x, _ ) -> x
      if not (null path')
      then do
        let con = sliceContents st r
        if V.null con
        then do
          let newSt = st { lastError = "Invalid address" }
          put newSt
          let err = if displayHelp newSt
                    then  (T.unpack . lastError $ newSt) ++ "\n?"
                    else "?"
          lift $ putStrLn  err
          lift $ hFlush stdout
        else do
          let lns = T.append (T.intercalate "\n" $ V.toList con) "\n"
          lift $ writeFile path' lns
          let newSt = st { dirty = False
                        , dirtyWarning = False
                        }
          put newSt
          (lift . print) $ T.length lns
      else do
        let newSt = st { lastError = "No current filename" }
        put newSt
        let err = if displayHelp newSt
                  then  (T.unpack . lastError $ newSt) ++ "\n?"
                  else "?"
        lift $ putStrLn  err
        lift $ hFlush stdout
      commandMode

-- |writeQuitAction writes addressed range to file and quits
writeQuitAction =
  Command "writeQuit" $ \r a ->
    Invocation writeQuitAction r a $ do
      st <- get
      let StringArg path = a
      let path' = case (T.unpack path, fileName st) of
                    ("", "") -> ""
                    ("", x) -> x
                    (x, _ ) -> x
      if not (null path')
      then do
        let con = sliceContents st r
        if V.null con
        then do
          let newSt = st { lastError = "Invalid address" }
          put newSt
          let err = if displayHelp newSt
                    then  (T.unpack . lastError $ newSt) ++ "\n?"
                    else "?"
          lift $ putStrLn  err
          lift $ hFlush stdout
          commandMode
        else do
          let lns = T.append (T.intercalate "\n" $ V.toList con) "\n"
          lift $ writeFile path' lns
          let newSt = st { dirty = False
                        , dirtyWarning = False
                        }
          put newSt
          (lift . print) $ T.length lns
          return ()
      else do
        let newSt = st { lastError = "No current filename" }
        put newSt
        let err = if displayHelp newSt
                  then  (T.unpack . lastError $ newSt) ++ "\n?"
                  else "?"
        lift $ putStrLn  err
        lift $ hFlush stdout
        commandMode

-- |appendFileAction appends address range to the end of a file
appendFileAction =
  Command "appendFile" $ \r a ->
    Invocation appendFileAction r a $ do
      st <- get
      let StringArg path = a
      let path' = case (T.unpack path, fileName st) of
                    ("", "") -> ""
                    ("", x) -> x
                    (x, _ ) -> x
      if not (null path')
      then do
        let con = sliceContents st r
        if V.null con
        then do
          let newSt = st { lastError = "Invalid address" }
          put newSt
          let err = if displayHelp newSt
                    then  (T.unpack . lastError $ newSt) ++ "\n?"
                    else "?"
          lift $ putStrLn  err
          lift $ hFlush stdout
        else do
          old <- lift $ readFile path'
          let newTxt = T.concat [old, T.intercalate "\n" $ V.toList con, "\n"]
          lift $ writeFile path' newTxt
          let newSt = st { dirty = False
                        , dirtyWarning = False
                        }
          put newSt
          (lift . print) $ (T.length newTxt) - (T.length old)
      else do
        let newSt = st { lastError = "No current filename" }
        put newSt
        let err = if displayHelp newSt
                  then  (T.unpack . lastError $ newSt) ++ "\n?"
                  else "?"
        lift $ putStrLn  err
        lift $ hFlush stdout
      commandMode

-- |putsAction pastes clipboard contents to location
putsAction =
  Command "puts" $ \r a ->
    Invocation putsAction r a $ do
        st <- get
        let results = checkClipboard st
                        >>= setToRangeM (Just r)
                        >>= moveClipboardToBuffer
                        >>= insertTextM
        case results of
          Left st' -> do
            printError st'
            put (clearTemps st')
          Right st' ->
            put (clearTemps st')
        commandMode
      where
        checkClipboard st =
          if V.null (clipboard st)
          then Left st { lastError = "Nothing to put" }
          else Right st
        moveClipboardToBuffer st =
          Right st { tBuffer = clipboard st }

-- |yankAction copies address range to clipboard
yankAction =
  Command "yank" $ \r a ->
    Invocation yankAction r a $ do
      st <- get
      let results = setFromRangeM r st
                      >>= sliceContentsM
      case results of
        Left st' -> do
          printError st'
          put (clearTemps st')
        Right st' ->
          put . clearTemps $ st' { clipboard = tBuffer st' }
      commandMode

-- |scrollAction prints N lines and moves position
scrollAction =
  Command "scroll" $ \r a ->
    Invocation scrollAction r a $ do
        st <- get
        let results = setFromRangeM r st
                        >>= setScrollLength (getNumberArgument a)
                        >>= sliceContentsM
        case results of
          Left st' -> do
            printError st'
            put (clearTemps st')
          Right st' -> do
            lift $ printBuff st'
            put . clearTemps $ st' { position = tFromEnd st' }
        commandMode
      where
        setScrollLength Nothing st = Right st { tFromEnd = tFromStart st + (lastScroll st) }
        setScrollLength (Just n) st =
          if n < 1
          then Right st { tFromEnd = tFromStart st + 1, lastScroll = 1 }
          else Right st { tFromEnd = tFromStart st + (n - 1), lastScroll = (n - 1) }

-- |bangAction
bangAction =
  Command "bang" $ \r a ->
    Invocation bangAction r a $ do
      st <- get
      case a of
        StringArg cmd -> lift $ catch (callCommand $ T.unpack cmd)
                                      (\e -> do let err = show (e :: IOException)
                                                hPutStr stderr ("Command Failure: " ++ err))
        _ -> do let newSt = st { lastError = "Invalid args" }
                printError newSt
                put (clearTemps newSt)
      commandMode

-- |commentAction
commentAction =
  Command "comment" $ \r a ->
    Invocation commentAction r a $ do
      commandMode

-- |lineNumberAction prints the addressed line's line number
lineNumberAction =
  Command "lineNumber" $ \r a ->
    Invocation lineNumberAction r a $ do
      st <- get
      let results = setFromRangeM r st
      case results of
        Left st' -> do
          printError st'
          put (clearTemps st')
        Right st' -> do
          lift $ print (tFromStart st')
          lift $ hFlush stdout
          put (clearTemps st')
      commandMode

-- |jumpAction jump to addressed location or next line
jumpAction =
  Command "jump" $ \r a ->
    Invocation jumpAction r a $ do
        st <- get
        let results = jumpRangeConvert r st
                        >>= sliceContentsM
                        >>= setPositionToFromM

        case results of
          Left st' -> do
            printError st'
            put (clearTemps st')
          Right st' -> do
            lift $ printBuff st'
            put $ clearTemps st'
        commandMode
      where
        jumpRangeConvert NoRange st = setFromRangeM (SingleRange $ RelativePosition Plus 1) st
        jumpRangeConvert (DoubleRange _ _) st = Left st { lastError = "Invalid address" }
        jumpRangeConvert singleRange st = setFromRangeM singleRange st

-- |commandListAction prints out the entire command list
commandListAction =
  Command "commandList" $ \r a ->
    Invocation commandListAction r a $ do
      st <- get
      case getResource "help.txt" of
        Just cont -> do
          lift $ Char8.putStrLn cont
          lift $ hFlush stdout
        Nothing -> do
          let newSt = st { lastError = "No help found" }
          put newSt
          printError newSt
      commandMode


-- |Helpers

-- |clearTemps resets all temp variables from state info
clearTemps :: StateInfo -> StateInfo
clearTemps st =
  st { tBuffer = V.empty
     , tFromStart = -1
     , tFromEnd = -1
     , tToStart = -1
     , tToEnd = -1
     }

-- |getPosition converts a Position to an int value in the Vector
-- Position is 1's based, with 0 being before the array and all values
-- going after
-- 0   1   2
--  |a| |b|
getPosition :: StateInfo -> Position -> Int
getPosition _ NoPosition = -1
getPosition s CurrentPosition = if position s == 0 && not (V.null (contents s))
                                then 1
                                else position s
getPosition s EndOfDoc = V.length $ contents s
getPosition s (AbsolutePosition p) | p < 0 = -1
                                   | p > V.length (contents s) = -1
                                   | otherwise = p
getPosition s (RelativePosition Minus p) | position s - p < 0 = -1
                                         | otherwise = position s - p
getPosition s (RelativePosition Plus p) | position s + p > V.length (contents s) = -1
                                        | otherwise = position s + p
getPosition s (NextLineMatching re) =
  if T.null re
    then findNext (position s) $ matchList (contents s) (T.unpack $ lastSearch s)
    else findNext (position s) $ matchList (contents s) (T.unpack re)
getPosition s (PreviousLineMatching re) =
  if T.null re
    then findPrev (position s) $ matchList (contents s) (T.unpack $ lastSearch s)
    else findPrev (position s) $ matchList (contents s) (T.unpack re)
getPosition s (MarkLocation c) =
  case V.find (\(x,_) -> x == c) (marks s) of
    Just (_, p) -> if p > V.length (contents s)
                   then -1
                   else p
    Nothing -> -1

matchList :: V.Vector T.Text -> String -> V.Vector Int
matchList v re = V.map mapFn $ V.filter filterFn (V.indexed v)
  where
    mapFn :: (Int, T.Text) -> Int
    mapFn (idx, _) = (idx+1) -- position starts at 1
    filterFn :: (Int, T.Text) -> Bool
    filterFn (_,txt) = fromMaybe False ((T.unpack txt) =~~ re)
--    filterFn (_,txt) = case (T.unpack txt) =~~ re of
--                        Just x -> x
--                        Nothing -> False
    idxVec = V.indexed v

findNext :: Int -> V.Vector Int -> Int
findNext pos v | not (V.null fwd) = V.head fwd
               | not (V.null bwd) = V.head bwd
               | otherwise =  -1
  where
    (fwd, bwd) = V.partition (>pos) v

findPrev :: Int -> V.Vector Int -> Int
findPrev pos v | not (V.null bwd) = V.last bwd
               | not (V.null fwd) = V.last fwd
               | otherwise =  -1
  where
    (fwd, bwd) = V.partition (>=pos) v

-- |sliceContents copies range from state's contents
sliceContents :: StateInfo -> Range -> V.Vector T.Text
sliceContents _ (SingleRange NoPosition) = V.empty
sliceContents s (SingleRange pos)
    | V.null con = V.empty
    | p == -1 = V.empty
    | p == 0 = V.slice 0 1 con
    | otherwise = V.slice (p-1) 1 con
  where
    con = contents s
    p = getPosition s pos
sliceContents s (DoubleRange start end)
    | V.null con = V.empty
    | startPos < 0 || endPos < 1 || startPos > endPos = V.empty
    | startPos == 0 = V.slice 0 endPos con
    | otherwise = V.slice (startPos - 1) (endPos - startPos + 1) con
  where
    con = contents s
    startPos = getPosition s start
    endPos = getPosition s end

-- |removeContentsM deletes lines assigned to the From registers
-- This returns Either StateInfo StateInfo assigning error message on failure
removeContentsM st
    | V.null con = Left st { lastError = "No contents to delete" }
    | startPos < 0 || endPos < 1 || startPos > endPos
        = Left st { lastError = "No contents to delete" }
    | startPos == 0 = Right st { contents = V.drop endPos con
                               , dirty = True
                               , dirtyWarning = False
                               }
    | otherwise = Right st { contents = V.take (startPos-1) con V.++ V.drop endPos con
                           , dirty = True
                           , dirtyWarning = False
                           }
  where
      con = contents st
      startPos = tFromStart st
      endPos = tFromEnd st
      toStart = tToStart st

-- |shiftToByFromM shifts the To addresses by the range of the From addresses
-- This returns Either StateInfo StateInfo assigning error message on failure
shiftToByFromM st
    | toEnd < toStart = Left st { lastError = "Invalid address" }
    | toEnd <= fromStart = Right st -- To---To From---From
    | toStart >= fromEnd = Right st { tToStart = toStart - fromWidth
                                    , tToEnd = toEnd - fromWidth
                                    }
    | otherwise = Left st { lastError = "Invalid destination" }
  where
    toStart = tToStart st
    toEnd = tToEnd st
    fromStart = tFromStart st
    fromEnd = tFromEnd st
    fromWidth = fromEnd - fromStart

-- |runEditor is the "main"
runEditor :: IO ()
runEditor = do
  _ <- execStateT commandMode initialState
  return ()

-- |printError prints the last error message and the "?"
printError st = do
  let err = if displayHelp st
            then  (T.unpack . lastError $ st) ++ "\n?"
            else "?"
  lift $ putStrLn  err
  lift $ hFlush stdout

-- |setFromRangeM assigns the From registers
-- This returns Either StateInfo StateInfo assigning error message on failure
setFromRangeM NoRange st = Right st { tFromStart = -1, tFromEnd = -1 }
setFromRangeM (SingleRange pos) st =
  case getPosition st pos of
    -1 -> Left st { lastError = "Invalid address" }
    x  -> case pos of
            NextLineMatching "" ->
              Right st { tFromStart = x, tFromEnd = x }
            NextLineMatching re ->
              Right st { tFromStart = x, tFromEnd = x, lastSearch = re }
            PreviousLineMatching "" ->
              Right st { tFromStart = x, tFromEnd = x }
            PreviousLineMatching re ->
              Right st { tFromStart = x, tFromEnd = x, lastSearch = re }
            _ -> Right st { tFromStart = x, tFromEnd = x }
setFromRangeM (DoubleRange start end) st =
  case (getPosition st start, getPosition st end) of
    (-1,_) -> Left st { lastError = "Invalid address" }
    (_,-1) -> Left st { lastError = "Invalid address" }
    (x,y) -> if x < y
             then Right st { tFromStart = x, tFromEnd = y }
             else Left st { lastError = "Invalid address" }

-- |setToRangeM assigns the To registers
-- This returns Either StateInfo StateInfo assigning error message on failure
setToRangeM Nothing st = Left st { lastError = "Invalid Argument" }
setToRangeM (Just NoRange) st = Right st { tToStart = -1, tToEnd = -1 }
setToRangeM (Just (SingleRange pos)) st =
  case getPosition st pos of
    -1 -> Left st { lastError = "Invalid address" }
    x  -> case pos of
            NextLineMatching "" ->
              Right st { tToStart = x, tToEnd = x }
            NextLineMatching re ->
              Right st { tToStart = x, tToEnd = x, lastSearch = re }
            PreviousLineMatching "" ->
              Right st { tToStart = x, tToEnd = x }
            PreviousLineMatching re ->
              Right st { tToStart = x, tToEnd = x, lastSearch = re }
            _ -> Right st { tToStart = x, tToEnd = x }
setToRangeM (Just (DoubleRange start end)) st =
  case (getPosition st start, getPosition st end) of
    (-1,_) -> Left st { lastError = "Invalid address" }
    (_,-1) -> Left st { lastError = "Invalid address" }
    (x,y) -> Right st { tToStart = x, tToEnd = y }

-- |checkDirtyM checks if the dirty flags are set and returns failure
-- This returns Either StateInfo StateInfo assigning error message on failure
checkDirtyM st =
  if dirty st && not (dirtyWarning st)
  then Left st { lastError = "Warning: buffer modified"
               , dirtyWarning = True
               }
  else Right st

-- |getStringArgument returns on string args
getStringArgument (StringArg s) = Just s
getStringArgument _ = Nothing

-- |getRangeArgument returns on range args
getRangeArgument (RangeArg r) = Just r
getRangeArgument _ = Nothing

-- |getNumberArgument returns on number args
getNumberArgument (NumberArg n) = Just n
getNumberArgument _ = Nothing

-- |getRegExArgument returns a RegEx arg
getRegExArgument (RegExArg re replace) = Just (re, replace)
getRegExArgument _ = Nothing

-- |sliceContentsM copies the From range to the temp buffer
-- This returns Either StateInfo StateInfo assigning error message on failure
sliceContentsM st
    | V.null con = Right st { tBuffer = V.empty 
                            , tFromStart = 0
                            , tFromEnd = 0
                            }
    | startPos < 0 || endPos < 1 || startPos > endPos
      = Left st { lastError = "Invalid address" } 
    | startPos == 0 = Right st { tBuffer = V.slice 0 endPos con
                               , tFromStart = 0
                               , tFromEnd = endPos
                               }
    | otherwise = Right st { tBuffer = V.slice (startPos - 1) (endPos - startPos + 1) con
                           , tFromStart = startPos
                           , tFromEnd = endPos
                           }
  where
    con = contents st
    startPos = tFromStart st
    endPos = if tFromEnd st > V.length con
             then V.length con
             else tFromEnd st

-- |insertTextM insert temp buffer into contents
-- This returns Either StateInfo StateInfo assigning error message on failure
insertTextM st =
  case (tToStart st, tBuffer st) of
    (-1, _) -> Left st { lastError = "Invalid address" }
    (0, con) -> Right st { contents = con V.++ contents st }
    (idx, con) -> Right st { contents = l V.++ con V.++ r }
                    where oldCon = contents st
                          l = V.take idx oldCon
                          r = V.drop idx oldCon

-- |insertText inserts text into contents
insertText :: StateInfo -> Int -> V.Vector T.Text -> V.Vector T.Text
insertText st (-1) _ = contents st
insertText st 0 con = con V.++ contents st
insertText st idx con = l V.++ con V.++ r
  where l = V.take idx (contents st)
        r = V.drop idx (contents st)

-- |printBuff prints the temp buffer to stdout
printBuff :: StateInfo -> IO ()
printBuff st = do
  V.mapM_ (putStrLn . T.unpack) (tBuffer st)
  hFlush stdout

-- |fixFromRangeM moves the From range for insert rather than append
-- This returns Either StateInfo StateInfo assigning error message on failure
fixFromRangeM st =
  case tFromStart st of
    (-1) -> Right st { tFromStart = 0 }
    0 -> Right st
    x -> Right st { tFromStart = x - 1 }

-- |fixToRangeM moves the To range for insert rahter than append
-- This returns Either StateInfo StateInfo assigning error message on failure
fixToRangeM st =
  case tToStart st of
    (-1) -> Right st { tToStart = 0 }
    0 -> Right st
    x -> Right st { tToStart = x - 1 }

-- |setPositionToFromM sets the position to the From start register
-- This returns Either StateInfo StateInfo assigning error message on failure
setPositionToFromM st
  | tFromStart st == (-1) = Left st { lastError = "Bad Position" }
  | otherwise = Right st { position = tFromStart st }

-- |setPositionToFromM sets the position to the To start register
-- This returns Either StateInfo StateInfo assigning error message on failure
setPositionToToM st
  | tToStart st == (-1) = Left st { lastError = "Bad Position" }
  | otherwise = Right st { position = tToStart st }
