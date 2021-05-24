{ pkgs, lib, options, inputs, ... }:

with lib;
{
  imports = [
    ./1password
    ./3d
    ./anki
    ./aws
    ./browser
    ./chromium
    ./clojure
    ./coq
    ./docker
    ./dogfood
    ./emacs
    ./freecad
    ./fonts
    ./git
    ./gnupg
    ./gtypist
    ./hammerspoon
    ./i3
    ./kakoune
    ./kitty
    ./linux
    ./linuxcnc
    ./macos
    ./modules/environment/xdg
    ./modules/bubbles
    ./networking/plan9
    ./networking/ses-email
    ./networking/ssh
    ./networking/tls
    ./networking/update-dns
    ./networking/wifi
    ./nix
    ./npm
    ./postgres
    ./rlwrap
    ./shell
    ./taskwarrior
    ./tex
    ./timeular
    ./tmux
    ./tracker
    ./usb-overdrive
    ./vnc
    ./weechat
    ./x
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
