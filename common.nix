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
    ./chromium
    ./clojure
    ./coq
    ./dogfood
    ./emacs
    ./fonts
    ./git
    ./gnupg
    ./gtypist
    ./i3
    ./kakoune
    ./kitty
    ./linux
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
    ./x
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
