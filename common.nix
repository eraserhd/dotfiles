{ pkgs, lib, options, inputs, ... }:

with lib;
{
  imports = [
    ./3d
    ./browser
    ./dogfood
    ./fonts
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
    ./programs/gnupg
    ./programs/go
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
    ./programs/timeular
    ./programs/tmux
    ./programs/usb-overdrive
    ./programs/weechat
    ./programs/x
    ./programs/zsh
    ./shell
  ];

  config = mkMerge [
    (if (builtins.hasAttr "systemPath" options.environment) then {
      environment.systemPath = [ (toString ./bin) ];
    } else {
      environment.variables.PATH = [ (toString ./bin) ];
    })
  ];
}
