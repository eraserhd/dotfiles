{ pkgs, lib, options, inputs, ... }:

with lib;
{
  imports = [
    ./3d
    ./browser
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
    ./programs/1password
    ./programs/anki
    ./programs/aws
    ./programs/buildkite
    ./programs/clojure
    ./programs/gtypist
    ./programs/i3
    ./programs/kakoune
    ./programs/kitty
    ./programs/npm
    ./programs/postgres
    ./programs/taskwarrior
    ./programs/tex
    ./programs/usb-overdrive
    ./programs/weechat
    ./rlwrap
    ./shell
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
