{ pkgs, lib, options, inputs, ... }:

with lib;
{
  imports = [
    ./1password
    ./3d
    ./anki
    ./browser
    ./buildkite
    ./chromium
    ./docker
    ./dogfood
    ./emacs
    ./freecad
    ./fonts
    ./git
    ./gnupg
    ./go
    ./hammerspoon
    ./i3
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
    ./programs/aws
    ./programs/clojure
    ./programs/gtypist
    ./programs/kakoune
    ./programs/kitty
    ./programs/npm
    ./programs/postgres
    ./programs/usb-overdrive
    ./programs/weechat
    ./rlwrap
    ./shell
    ./taskwarrior
    ./tex
    ./timeular
    ./tmux
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
