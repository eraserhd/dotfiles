{ lib, ... }:

with lib;
{
  options.local.terminalPopupCommand = mkOption {
    type = types.str;
    example = "kitty-popup";
    description = ''
      A command line which runs a command in a popup terminal window, as
      `<this> NAME COMMAND [ARG...]`.  The window is called NAME, which is its
      title, and its class where there are classes, for the window manager to
      float it by.  The terminal's module sets it.
    '';
  };
}
