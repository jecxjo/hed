# hed - An ed(itor) clone written in Haskell

This project is (going to be) an editor somewhere between ed and ex. Supports
many of the same commands, with slight modifications like PCRE regex support
instead of POSIX.

## Commands

Commands are laid out as `{range}{action}{argument}`

Line Addressing:

- Current Position: `.`
- End of Doc: `$`
- Absolute: `1`
- Relative: `-1` `+3`
- Marks: `'a`
- (PCRE) Search: `/foo/` `?bar?`

Special addressing:

- Entire Document: `%` or `,`
- Current line to end: `;`

Supported Commands:

- Append: `(.)a`
- Change: `(.,.)c`
- Delete: `(.,.)d`
- Edit: `e <filename>` or `E <filename>`
- Default File: `f <filename>`
- Help: `h` or `H`
- Insert: `(.)i`
- Join: `(.,+1)j`
- Mark: `ka`
- Print: `(.,.)l` or `(.,.)n` or `(.,.)p`
- Move: `(.,.)m(.)`
- Quit: `q` or `Q`
- Read: `(.)r <filename>`
- Copy: `(.,.)t(.)`
- Write: `(1,$)w <filename>` or `(1,$)wq <filename>`
- Append: `(1,$)W <filename>`
- Yank: `(.,.)y`
- Put: `(.)x`
- Scroll: `(+1)z<count>`
- Execute Command: `!<shell command>`
- Comment: `(.,.)# <comment text>`
- Line Number: `(.)=`
- Jump: `(.)`
- Swap: `(.,.)s/regex/replace/`
- Swap Last: `(.,.)s`
- Command List: `?`

## Arguments

_Todo_
