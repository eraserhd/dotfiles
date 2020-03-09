{ pkgs, lib, options, ... }:

with lib;
{
  imports = [
    ./aws
    ./browser
    ./clojure
    ./coq
    ./dogfood
    ./elvish
    ./emacs
    ./git
    ./gnupg
    ./gtypist
    ./kakoune
    ./networking/plan9
    ./networking/ses-email
    ./networking/ssh
    ./networking/tls
    ./networking/update-dns
    ./npm
    ./rlwrap
    ./scheme
    ./senzia
    ./shell
    ./skhd
    ./taskwarrior
    ./tex
    ./tmux
    ./tracker
    ./vim
    ./weechat
    ./zsh
  ];

  config = mkMerge [
    (if (builtins.hasAttr "systemPath" options.environment) then {
      environment.systemPath = [ (toString ./bin) ];
    } else {
      environment.variables.PATH = [ (toString ./bin) ];
    })
  ];
}
