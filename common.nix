{ pkgs, lib, options, ... }:

with lib;
{
  imports = [
    ./1password
    ./aws
    ./browser
    ./clojure
    ./coq
    ./dogfood
    ./emacs
    ./git
    ./gnupg
    ./gtypist
    ./kakoune
    ./kitty
    ./modules/environment/xdg
    ./networking/plan9
    ./networking/ses-email
    ./networking/ssh
    ./networking/tls
    ./networking/update-dns
    ./networking/wifi
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
    ./yabai
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
