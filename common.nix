{ pkgs, lib, options, inputs, ... }:

with lib;
{
  imports = [
    ./1password
    ./3d
    ./anki
    ./aws
    ./browser
    ./buildkite
    ./chromium
    ./clojure
    ./docker
    ./dogfood
    ./emacs
    ./freecad
    ./fonts
    ./git
    ./gnupg
    ./go
    ./gtypist
    ./hammerspoon
    ./i3
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
    ./nexus
    ./nix
    ./npm
    ./postgres
    ./programs/kakoune
    ./rlwrap
    ./shell
    ./taskwarrior
    ./tex
    ./timeular
    ./tmux
    ./usb-overdrive
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
