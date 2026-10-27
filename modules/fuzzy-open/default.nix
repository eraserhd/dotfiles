{ lib, ... }:

with lib;
{
  options.local.fuzzyOpenCommand = mkOption {
    type = types.str;
    example = "broot-popup";
    description = ''
      A command line which opens a popup window in which to pick a file to
      open.  The window is called `fuzzy-open`, which is its title, and its
      class where there are classes, for the window manager to float it by.
      It may be given a directory to start in.
    '';
  };
}
