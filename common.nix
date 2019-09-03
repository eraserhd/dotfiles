{ pkgs, lib, options, ... }:

with lib;
{
  imports = [
    ./alacritty
    ./aws
    ./bash
    ./clojure
    ./git
    ./gtypist
    ./kak
    ./npm
    ./plan9
    ./rlwrap
    ./tex
    ./tmux
    ./weechat
  ];

  config = mkMerge [
    {
      environment.variables = {
        CDPATH = [
          "."
          "~/src"
          "~/src/dotfiles/kak/config/kak.symlink/autoload"
        ];
      };
    }
    (if (builtins.hasAttr "systemPath" options.environment) then {
      environment.systemPath = [ (toString ./bin) ];
    } else {
      environment.variables.PATH = [ (toString ./bin) ];
    })
  ];
}
