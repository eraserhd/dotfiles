# desertex theme

# red            rgb:ff5370
# light_red      rgb:ff869a
# dark_red       rgb:be5046
# green          rgb:c3e88d
# yellow         rgb:ffcb6b
# dark_yellow    rgb:f78c6c
# blue           rgb:82b1ff
# purple         rgb:c792ea
# cyan           rgb:89ddff
# white          rgb:bfc7d5
# black          rgb:292d3e
# comment_grey   rgb:697098
# gutter_fg_grey rgb:4b5263
# cursor_grey    rgb:2c323c
# visual_grey    rgb:3e4452
# menu_grey      rgb:3e4452
# special_grey   rgb:3b4048
# vertsplit      rgb:181a1f
# visual_black   default

# Code
face global value      rgb:f78c6c
face global type       rgb:ffcb6b
face global function   rgb:82b1ff
face global variable   rgb:82b1ff
face global identifier rgb:82b1ff
face global string     rgb:c3e88d
face global error      rgb:c3bf9f+b
face global keyword    rgb:c792ea
face global operator   rgb:89ddff
face global attribute  rgb:eedc82
face global comment    rgb:697098+i

# #include <...>
face global meta       rgb:ffcb6b

# Markup
face global title  rgb:82b1ff
face global header rgb:89ddff
face global bold   rgb:ff5370
face global italic rgb:ffcb6b
face global mono   rgb:c3e88d
face global block  rgb:c792ea
face global link   rgb:89ddff
face global bullet rgb:89ddff
face global list   rgb:ffcb6b

# Builtin
# fg,bg+attributes # 36,40,56
# face global Default default,rgb:292d3e <- change the terminal bg color instead
face global Default            rgb:bfc7d5,rgb:313348

face global PrimarySelection   rgb:292d3e,rgb:ffffff+fg
face global SecondarySelection rgb:292d3e,rgb:bfc7d5

face global PrimaryCursor      white,rgb:c792ea+bfg
face global SecondaryCursor    black,rgb:c792ea+fg

face global PrimaryCursorEol   black,rgb:c3e88d+fg
face global SecondaryCursorEol black,rgb:c3e88d+fg

face global LineNumbers        rgb:4b5263
face global LineNumberCursor   rgb:ffcb6b,default+b

# Bottom menu:
# text + background
face global MenuBackground     black,rgb:c2bfa5+b
face global MenuForeground     rgb:292d3e,rgb:c792ea

# completion menu info
face global MenuInfo           white,rgb:445599

# assistant, [+]
face global Information        rgb:bfc7d5,rgb:3e4452

face global Error              rgb:bfc7d5,rgb:ff5370
face global StatusLine         rgb:bfc7d5,default

# Status line
face global StatusLineMode     rgb:292d3e,rgb:c792ea # insert, prompt, enter key ...
face global StatusLineInfo     rgb:bfc7d5,rgb:3e4452 # 1 sel
face global StatusLineValue    rgb:3e4452,rgb:c3e88d # param=value, reg=value. ex: "ey
face global StatusCursor       white,rgb:c792ea+bg

face global Prompt             white,rgb:c792ea+F # :
face global MatchingChar       rgb:ff5370+b       # (), {}
face global BufferPadding      rgb:82b1ff,default # EOF tildas (~)

# Whitespace characters
face global Whitespace         rgb:4b5263,default+fg
