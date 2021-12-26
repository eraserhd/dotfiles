{ pkgs, lib, options, inputs, ... }:

with lib;
{
  imports = [
    ./3d
    ./browser
    ./dogfood
    ./fonts
    ./gnupg
    ./go
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
    ./programs/chromium
    ./programs/clojure
    ./programs/docker
    ./programs/emacs
    ./programs/freecad
    ./programs/git
    ./programs/gtypist
    ./programs/hammerspoon
    ./programs/i3
    ./programs/kakoune
    ./programs/kitty
    ./programs/npm
    ./programs/postgres
    ./programs/rlwrap
    ./programs/taskwarrior
    ./programs/tex
    ./programs/usb-overdrive
    ./programs/weechat
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
