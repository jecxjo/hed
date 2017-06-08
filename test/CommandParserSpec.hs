{-# LANGUAGE OverloadedStrings #-}
module CommandParserSpec where

import CommandParser
import Commands


import Text.Parsec.Prim (parse)
import Test.Hspec ( Spec
                  , describe
                  , it
                  , shouldBe
                  , shouldNotBe
                  , context
                  )

-- |Required for auto-discovery
spec :: Spec
spec =
  describe "Parsers" $ do
    describe "Ranges" $ do
      context "Single Position" $ do
        it "matches current position (.)" $ do
          parse parseRange "" "." `shouldBe` Right (SingleRange CurrentPosition)

        it "matches end of doc ($)" $ do
          parse parseRange "" "$" `shouldBe` Right (SingleRange EndOfDoc)

        it "matches absolute position (2)" $ do
          parse parseRange "" "2" `shouldBe` Right (SingleRange $ AbsolutePosition 2)

        it "matches absolute position (234)" $ do
          parse parseRange "" "234" `shouldBe` Right (SingleRange $ AbsolutePosition 234)

        it "matches relative position (+1)" $ do
          parse parseRange "" "+1" `shouldBe` Right (SingleRange $ RelativePosition Plus 1)

        it "matches relative position (+20)" $ do
          parse parseRange "" "+20" `shouldBe` Right (SingleRange $ RelativePosition Plus 20)

        it "matches relative position (-1)" $ do
          parse parseRange "" "-1" `shouldBe` Right (SingleRange $ RelativePosition Minus 1)

        it "matches relative position (+1)" $ do
          parse parseRange "" "-20" `shouldBe` Right (SingleRange $ RelativePosition Minus 20)

        it "matches marked position ('a)" $ do
          parse parseRange "" "'a" `shouldBe` Right (SingleRange $ MarkLocation 'a')

        it "matches single relative (+)" $ do
          parse parseRange "" "+" `shouldBe` Right (SingleRange $ RelativePosition Plus 1)

        it "matches single relative (-)" $ do
          parse parseRange "" "-" `shouldBe` Right (SingleRange $ RelativePosition Minus 1)

      context "Multiple Position" $ do
        it "matches current, end (.,$)" $ do
          parse parseRange "" ".,$" `shouldBe` Right (DoubleRange CurrentPosition EndOfDoc)

        it "matches Absolute, Absolute (12,34)" $ do
          parse parseRange "" "12,34" `shouldBe` Right (DoubleRange (AbsolutePosition 12) (AbsolutePosition 34))

        it "matches Relative, Relative (-12,+34)" $ do
          parse parseRange "" "-12,+34" `shouldBe` Right (DoubleRange (RelativePosition Minus 12) (RelativePosition Plus 34))

        it "matches marked position ('a,'b)" $ do
          parse parseRange "" "'a,'b" `shouldBe` Right (DoubleRange (MarkLocation 'a') (MarkLocation 'b'))

        it "matches single relatives (-,+)" $ do
          parse parseRange "" "-,+" `shouldBe` Right (DoubleRange (RelativePosition Minus 1) (RelativePosition Plus 1))

        it "matches with whitespace ( 123  , 45 )" $ do
          parse parseRange "" " 123  , 45 " `shouldBe` Right (DoubleRange (AbsolutePosition 123) (AbsolutePosition 45))

    describe "Commands" $ do
      describe "append" $ do
        it "matches append without range (a)" $ do
          parse parseCommand "" "a" `shouldBe` Right (ParsedCommand "append" (SingleRange CurrentPosition) NoArgs)

        it "matches append with range (1a)" $ do
          parse parseCommand "" "1a" `shouldBe` Right (ParsedCommand "append" (SingleRange (AbsolutePosition 1)) NoArgs)

      describe "change" $ do
        it "changes no line (c)" $ do
          parse parseCommand "" "c" `shouldBe` Right (ParsedCommand "change" (SingleRange CurrentPosition) NoArgs)

        it "changes one line (.c)" $ do
          parse parseCommand "" ".c" `shouldBe` Right (ParsedCommand "change" (SingleRange CurrentPosition) NoArgs)

        it "changes no line (1,$c)" $ do
          parse parseCommand "" "1,$c" `shouldBe` Right (ParsedCommand "change" (DoubleRange (AbsolutePosition 1) EndOfDoc) NoArgs)

      describe "delete" $ do
        it "deletes no line (d)" $ do
          parse parseCommand "" "d" `shouldBe` Right (ParsedCommand "delete" (SingleRange CurrentPosition) NoArgs)

        it "deletes one line (.d)" $ do
          parse parseCommand "" ".d" `shouldBe` Right (ParsedCommand "delete" (SingleRange CurrentPosition) NoArgs)

        it "deletes no line (1,$d)" $ do
          parse parseCommand "" "1,$d" `shouldBe` Right (ParsedCommand "delete" (DoubleRange (AbsolutePosition 1) EndOfDoc) NoArgs)

      describe "edit" $ do
        it "edits simple file (e bar)" $ do
          parse parseCommand "" "e bar" `shouldBe` Right (ParsedCommand "edit" NoRange $ StringArg "bar")

        it "edits file (e /foo/bar.txt)" $ do
          parse parseCommand "" "e /foo/bar.txt" `shouldBe` Right (ParsedCommand "edit" NoRange $ StringArg "/foo/bar.txt")

        it "edits unconditional file (E /foo/bar.txt)" $ do
          parse parseCommand "" "E /foo/bar.txt" `shouldBe` Right (ParsedCommand "editUncond" NoRange $ StringArg "/foo/bar.txt")

      describe "default filename" $ do
        it "sets default file name (f bar.txt)" $ do
          parse parseCommand "" "f bar.txt" `shouldBe` Right (ParsedCommand "defaultFile" NoRange $ StringArg "bar.txt")

      -- describe "global" $ do

      describe "help" $ do
        it "last error help (h)" $ do
          parse parseCommand "" "h" `shouldBe` Right (ParsedCommand "help" NoRange NoArgs)

        it "toggles help (H)" $ do
          parse parseCommand "" "H" `shouldBe` Right (ParsedCommand "toggleHelp" NoRange NoArgs)

      describe "insert" $ do
        it "matches insert without range (a)" $ do
          parse parseCommand "" "i" `shouldBe` Right (ParsedCommand "insert" (SingleRange CurrentPosition) NoArgs)

        it "matches insert with range (1a)" $ do
          parse parseCommand "" "1i" `shouldBe` Right (ParsedCommand "insert" (SingleRange (AbsolutePosition 1)) NoArgs)

      describe "join" $ do
        it "matches join without range (j)" $ do
          parse parseCommand "" "j" `shouldBe` Right (ParsedCommand "join" (DoubleRange CurrentPosition (RelativePosition Plus 1)) NoArgs)

        it "matches join multi range (2,4j)" $ do
          parse parseCommand "" "2,4j" `shouldBe` Right (ParsedCommand "join" (DoubleRange (AbsolutePosition 2) (AbsolutePosition 4)) NoArgs)

      describe "mark" $ do
        it "matches no position (ka)" $ do
          parse parseCommand "" "ka" `shouldBe` Right (ParsedCommand "mark" (SingleRange CurrentPosition) $ StringArg "a")

        it "matches lined position (2ka)" $ do
          parse parseCommand "" "2ka" `shouldBe` Right (ParsedCommand "mark" (SingleRange (AbsolutePosition 2)) $ StringArg "a")

      describe "list" $ do
        it "matches no position (l)" $ do
          parse parseCommand "" "l" `shouldBe` Right (ParsedCommand "list" (SingleRange CurrentPosition) NoArgs)

        it "matches single postion (1l)" $ do
          parse parseCommand "" "1l" `shouldBe` Right (ParsedCommand "list" (SingleRange (AbsolutePosition 1)) NoArgs)

        it "matches no psotion (1,2l)" $ do
          parse parseCommand "" "1,2l" `shouldBe` Right (ParsedCommand "list" (DoubleRange (AbsolutePosition 1) (AbsolutePosition 2)) NoArgs)

      describe "move" $ do
        it "matches no position on both sides (m)" $ do
          parse parseCommand "" "m" `shouldBe` Right (ParsedCommand "move" (SingleRange CurrentPosition) (RangeArg (SingleRange CurrentPosition)))

        it "matches two single locations (1m2)" $ do
          parse parseCommand "" "1m2" `shouldBe` Right (ParsedCommand "move" (SingleRange $ AbsolutePosition 1) (RangeArg $ SingleRange $ AbsolutePosition 2))

        it "matches range locations (1,2m4)" $ do
          parse parseCommand "" "1,2m4" `shouldBe` Right (ParsedCommand "move" (DoubleRange (AbsolutePosition 1) (AbsolutePosition 2)) (RangeArg $ SingleRange $ AbsolutePosition 4))

      describe "numbered" $ do
        it "matches no position (n)" $ do
          parse parseCommand "" "n" `shouldBe` Right (ParsedCommand "numbered" (SingleRange CurrentPosition) NoArgs)

        it "matches single postion (1n)" $ do
          parse parseCommand "" "1n" `shouldBe` Right (ParsedCommand "numbered" (SingleRange (AbsolutePosition 1)) NoArgs)

        it "matches no psotion (1,2n)" $ do
          parse parseCommand "" "1,2n" `shouldBe` Right (ParsedCommand "numbered" (DoubleRange (AbsolutePosition 1) (AbsolutePosition 2)) NoArgs)

      describe "print" $ do
        it "matches no position (p)" $ do
          parse parseCommand "" "p" `shouldBe` Right (ParsedCommand "print" (SingleRange CurrentPosition) NoArgs)

        it "matches single postion (1p)" $ do
          parse parseCommand "" "1p" `shouldBe` Right (ParsedCommand "print" (SingleRange (AbsolutePosition 1)) NoArgs)

        it "matches no psotion (1,2p)" $ do
          parse parseCommand "" "1,2p" `shouldBe` Right (ParsedCommand "print" (DoubleRange (AbsolutePosition 1) (AbsolutePosition 2)) NoArgs)

      describe "togglePrompt" $ do
        it "matches command (P)" $ do
          parse parseCommand "" "P" `shouldBe` Right (ParsedCommand "togglePrompt" NoRange NoArgs)

      describe "quit" $ do
        it "matches quit (q)" $ do
          parse parseCommand "" "q" `shouldBe` Right (ParsedCommand "quit" NoRange NoArgs)

        it "matches quit unconditional (Q)" $ do
          parse parseCommand "" "Q" `shouldBe` Right (ParsedCommand "quitUncond" NoRange NoArgs)

      describe "read" $ do
        it "matches no location (r /foo/bar.txt)" $ do
          parse parseCommand "" "r /foo/bar.txt" `shouldBe` Right (ParsedCommand "read" (SingleRange EndOfDoc) $ StringArg "/foo/bar.txt")

        it "matches location (.r /foo/bar.txt)" $ do
          parse parseCommand "" ".r /foo/bar.txt" `shouldBe` Right (ParsedCommand "read" (SingleRange CurrentPosition) $ StringArg "/foo/bar.txt")

      -- describe "swap" $ do

      describe "copy" $ do
        it "matches no location (t)" $ do
          parse parseCommand "" "t" `shouldBe` Right (ParsedCommand "copy" (SingleRange CurrentPosition) $ RangeArg $ SingleRange CurrentPosition)

        it "matches single locations (1t2)" $ do
          parse parseCommand "" "1t2" `shouldBe` Right (ParsedCommand "copy" (SingleRange $ AbsolutePosition 1) $ RangeArg $ SingleRange $ AbsolutePosition 2)

        it "matches range location (1,2t3)" $ do
          parse parseCommand "" "1,2t3" `shouldBe` Right (ParsedCommand "copy" (DoubleRange (AbsolutePosition 1) (AbsolutePosition 2)) $ RangeArg $ SingleRange $ AbsolutePosition 3)

      describe "undo" $ do
        it "matches (u)" $ do
          parse parseCommand "" "u" `shouldBe` Right (ParsedCommand "undo" NoRange NoArgs)

      -- describe "reverseGlobal"

      describe "write" $ do
        it "matches no location (w /foo/bar.txt)" $ do
          parse parseCommand "" "w /foo/bar.txt" `shouldBe` Right (ParsedCommand "write" (DoubleRange (AbsolutePosition 1) EndOfDoc) $ StringArg "/foo/bar.txt")

        it "matches single locations (1w /foo/bar.txt)" $ do
          parse parseCommand "" "1w /foo/bar.txt" `shouldBe` Right (ParsedCommand "write" (SingleRange $ AbsolutePosition 1) $ StringArg "/foo/bar.txt")

        it "matches range location (1,2w /foo/bar.txt)" $ do
          parse parseCommand "" "1,2w /foo/bar.txt" `shouldBe` Right (ParsedCommand "write" (DoubleRange (AbsolutePosition 1) (AbsolutePosition 2)) $ StringArg "/foo/bar.txt")

      describe "writeQuit" $ do
        it "matches no location (wq /foo/bar.txt)" $ do
          parse parseCommand "" "wq /foo/bar.txt" `shouldBe` Right (ParsedCommand "writeQuit" (DoubleRange (AbsolutePosition 1) EndOfDoc) $ StringArg "/foo/bar.txt")

        it "matches single locations (1wq /foo/bar.txt)" $ do
          parse parseCommand "" "1wq /foo/bar.txt" `shouldBe` Right (ParsedCommand "writeQuit" (SingleRange $ AbsolutePosition 1) $ StringArg "/foo/bar.txt")

        it "matches range location (1,2wq /foo/bar.txt)" $ do
          parse parseCommand "" "1,2wq /foo/bar.txt" `shouldBe` Right (ParsedCommand "writeQuit" (DoubleRange (AbsolutePosition 1) (AbsolutePosition 2)) $ StringArg "/foo/bar.txt")

      describe "appendFile" $ do
        it "matches no location (W /foo/bar.txt)" $ do
          parse parseCommand "" "W /foo/bar.txt" `shouldBe` Right (ParsedCommand "appendFile" (DoubleRange (AbsolutePosition 1) EndOfDoc) $ StringArg "/foo/bar.txt")

        it "matches single locations (1W /foo/bar.txt)" $ do
          parse parseCommand "" "1W /foo/bar.txt" `shouldBe` Right (ParsedCommand "appendFile" (SingleRange $ AbsolutePosition 1) $ StringArg "/foo/bar.txt")

        it "matches range location (1,2W /foo/bar.txt)" $ do
          parse parseCommand "" "1,2W /foo/bar.txt" `shouldBe` Right (ParsedCommand "appendFile" (DoubleRange (AbsolutePosition 1) (AbsolutePosition 2)) $ StringArg "/foo/bar.txt")

      describe "puts" $ do
        it "matches puts without range (x)" $ do
          parse parseCommand "" "x" `shouldBe` Right (ParsedCommand "puts" (SingleRange CurrentPosition) NoArgs)

        it "matches puts with range (1x)" $ do
          parse parseCommand "" "1x" `shouldBe` Right (ParsedCommand "puts" (SingleRange (AbsolutePosition 1)) NoArgs)

      describe "yank" $ do
        it "matches no range (y)" $ do
          parse parseCommand "" "y" `shouldBe` Right (ParsedCommand "yank" (SingleRange CurrentPosition) NoArgs)

        it "matches one range (1y)" $ do
          parse parseCommand "" "1y" `shouldBe` Right (ParsedCommand "yank" (SingleRange $ AbsolutePosition 1) NoArgs)

        it "matches no range (1,2y)" $ do
          parse parseCommand "" "1,2y" `shouldBe` Right (ParsedCommand "yank" (DoubleRange (AbsolutePosition 1) (AbsolutePosition 2))  NoArgs)

      describe "scroll" $ do
        it "matches no location (z2)" $ do
          parse parseCommand "" "z2" `shouldBe` Right (ParsedCommand "scroll" (SingleRange $ RelativePosition Plus 1) $ NumberArg 2)

        it "matches location (3z2)" $ do
          parse parseCommand "" "3z2" `shouldBe` Right (ParsedCommand "scroll" (SingleRange $ AbsolutePosition 3) $ NumberArg 2)

      describe "bang" $ do
        it "matches command with args (!cmd --args)" $ do
          parse parseCommand "" "!cmd --args" `shouldBe` Right  (ParsedCommand "bang" NoRange $ StringArg "cmd --args")

      describe "comment" $ do
        it "matches no range and no text(#)" $ do
          parse parseCommand "" "#" `shouldBe` Right (ParsedCommand "comment" (SingleRange CurrentPosition) $ StringArg "")

        it "matches no range (# this is a comment)" $ do
          parse parseCommand "" "# this is a comment" `shouldBe` Right (ParsedCommand "comment" (SingleRange CurrentPosition) $ StringArg " this is a comment")

        it "matches one range (1#)" $ do
          parse parseCommand "" "1#" `shouldBe` Right (ParsedCommand "comment" (SingleRange $ AbsolutePosition 1) $ StringArg "")

        it "matches no range (1,2#)" $ do
          parse parseCommand "" "1,2#" `shouldBe` Right (ParsedCommand "comment" (DoubleRange (AbsolutePosition 1) (AbsolutePosition 2)) $ StringArg "")

      describe "lineNumber" $ do
        it "matches no range (=)" $ do
          parse parseCommand "" "=" `shouldBe` Right (ParsedCommand "lineNumber" (SingleRange EndOfDoc) NoArgs)

        it "matches range (.=)" $ do
          parse parseCommand "" ".=" `shouldBe` Right (ParsedCommand "lineNumber" (SingleRange CurrentPosition) NoArgs)

      describe "jump" $ do
        it "jumps with no range (<newline>)" $ do
          parse parseCommand "" "" `shouldBe` Right (ParsedCommand "jump" NoRange NoArgs)

        it "jumps with range (3<newline>)" $ do
          parse parseCommand "" "3" `shouldBe` Right (ParsedCommand "jump" (SingleRange $ AbsolutePosition 3) NoArgs)
