import XMonad
import XMonad.Actions.FocusNth
import XMonad.Actions.Navigation2D
import XMonad.Actions.UpdatePointer
import XMonad.Layout.SimpleDecoration
import XMonad.Util.EZConfig
import XMonad.Util.Paste
import XMonad.Util.SpawnOnce
import XMonad.Util.Themes
import qualified XMonad.StackSet as W

data DevLayout a = DevLayout deriving (Read, Show)

instance LayoutClass DevLayout a where
    pureLayout DevLayout rect stack =
        zip windows rectangles
      where
        windows = W.integrate stack
        n = length windows
        rectangles = if n < 4
                     then splitHorizontally n rect
                     else let cols = splitHorizontally 4 rect
                          in (take 3 cols) ++ (splitVertically (n-3) (last cols))

    pureMessage _ _ = Nothing

myRed        = "#ff5370"
myBlack      = "#292d3e"
myYellow     = "#ffcb6b"
myDarkYellow = "#f78c6c"
myPurple     = "#c792ea"
myVisualGrey = "#3e4452"

myTheme :: Theme
myTheme = def { fontName            = "xft:mononoki-10"
              , activeColor         = myBlack
              , inactiveColor       = myBlack
              , urgentColor         = myBlack
              , activeTextColor     = myDarkYellow
              , inactiveTextColor   = myPurple
              , urgentTextColor     = myRed
              , decoWidth           = 25
              , decoHeight          = 16
              }

myLayout = simpleDeco shrinkText myTheme (DevLayout ||| Full)

sigils = ["a", "b", "c", "d", "e", "g", "i", "m", "n", "o", "p", "q", "r", "s", "t", "u", "v", "w", "x", "y", "z"]

main :: IO ()
main = xmonad $ withNavigation2DConfig def $ def
  { modMask            = mod4Mask  -- se Command/Super for mod
  , borderWidth        = 2
  , normalBorderColor  = myVisualGrey
  , focusedBorderColor = myDarkYellow
  , terminal           = "kitty"
  , layoutHook         = myLayout
  , logHook            = updatePointer (0.5, 0.5) (0, 0)
  , startupHook        = do
      spawnNOnOnce 5 "workspace1" "kitty"
      spawnOnce "firefox"
  }
 `additionalKeysP`
  ([ ("C-w h", windowGo L False)
   , ("C-w j", windowGo D False)
   , ("C-w k", windowGo U False)
   , ("C-w l", windowGo R False)
   , ("C-w M1-h", windowSwap L False)
   , ("C-w M1-j", windowSwap D False)
   , ("C-w M1-k", windowSwap U False)
   , ("C-w M1-l", windowSwap R False)

   -- ("C-w ,", ...) --FIXME:
   -- ("C-w .", pasteChar controlMask 'W') -- doesn't work
   ] ++
   [ ("C-w "++sigil, focusNth i) | (i, sigil) <- zip [0..] sigils ] ++
   [ ("C-w M1-"++sigil, swapNth i) | (i, sigil) <- zip [0..] sigils ])
