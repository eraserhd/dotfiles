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
    ./hammerspoon
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
    ./taskwarrior
    ./tex
    ./tmux
    ./tracker
    ./usb-overdrive
    ./vim
    ./vnc
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
