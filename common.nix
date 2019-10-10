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
    #./networking/nagios
    ./networking/plan9
    ./networking/ses-email
    ./networking/ssh
    ./networking/update-dns
    ./npm
    ./rlwrap
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
