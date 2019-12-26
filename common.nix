{ pkgs, lib, options, ... }:

with lib;
{
  imports = [
    ./aws
    ./bash
    ./browser
    ./clojure
    ./coq
    ./dogfood
    ./emacs
    ./git
    ./gnupg
    ./gtypist
    ./kakoune
    ./lib
    ./networking/plan9
    ./networking/ses-email
    ./networking/ssh
    ./networking/update-dns
    ./npm
    ./rlwrap
    ./taskwarrior
    ./tex
    ./tmux
    ./tracker
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
