import XMonad
import XMonad.Actions.Navigation2D
import XMonad.Actions.UpdatePointer
import XMonad.Util.EZConfig
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

myLayout = DevLayout ||| Full

sigils = ["a", "b", "c", "d", "e", "g", "i", "m", "n", "o", "p", "q", "r", "s", "t", "u", "v", "w", "x", "y", "z"]

main :: IO ()
main = xmonad $ withNavigation2DConfig def $ def
  { modMask = mod4Mask  -- se Command/Super for mod
  , terminal = "kitty"
  , layoutHook = myLayout
  , logHook = updatePointer (0.5, 0.5) (0, 0)
  }
 `additionalKeysP`
  [ ("C-w h", windowGo L False)
  , ("C-w j", windowGo D False)
  , ("C-w k", windowGo U False)
  , ("C-w l", windowGo R False)
  , ("C-w M1-h", windowSwap L False)
  , ("C-w M1-j", windowSwap D False)
  , ("C-w M1-k", windowSwap U False)
  , ("C-w M1-l", windowSwap R False)
  ]
