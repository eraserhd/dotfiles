{ pkgs, lib, options, ... }:

with lib;
{
  imports = [
    ./alacritty
    ./aws
    ./bash
    ./clojure
    ./emacs
    ./git
    ./gnupg
    ./gtypist
    ./kak
    ./karabiner
    ./lib
    ./mail
    ./networking/plan9
    ./networking/update-dns
    ./npm
    ./rlwrap
    ./ssh
    ./tex
    ./tmux
    ./vim
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
