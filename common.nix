{ pkgs, lib, options, ... }:

with lib;
{
  imports = [
    ./1password
    ./3d
    ./alfred
    ./anki
    ./aws
    ./browser
    ./clojure
    ./coq
    ./dogfood
    ./emacs
    ./fonts
    ./git
    ./gnupg
    ./gtypist
    ./kakoune
    ./kitty
    ./i3
    ./macos
    ./modules/environment/xdg
    ./modules/bubbles
    ./networking/plan9
    ./networking/ses-email
    ./networking/ssh
    ./networking/tls
    ./networking/update-dns
    ./networking/wifi
    ./npm
    ./rlwrap
    ./scheme
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
