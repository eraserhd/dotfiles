{ pkgs, lib, options, inputs, ... }:

with lib;
{
  imports = [
    ./browser
    ./dogfood
    ./fonts
    ./modules/bubbles
    ./modules/environment/xdg
    ./networking
    ./networking/ses-email
    ./networking/ssh
    ./networking/tls
    ./networking/update-dns
    ./networking/wifi
    ./os/common
    ./pkg
    ./programs/1password
    ./programs/anki
    ./programs/aws
    ./programs/buildkite
    ./programs/chromium
    ./programs/clojure
    ./programs/cue
    ./programs/discord
    ./programs/docker
    ./programs/emacs
    ./programs/freecad
    ./programs/git
    ./programs/gnupg
    ./programs/gnuplot
    ./programs/go
    ./programs/gtypist
    ./programs/hammerspoon
    ./programs/i3
    ./programs/kakoune
    ./programs/kitty
    ./programs/kubernetes
    ./programs/nats
    ./programs/nexus
    ./programs/nix
    ./programs/npm
    ./programs/openscad
    ./programs/pop
    ./programs/postgres
    ./programs/rlwrap
    ./programs/slack
    ./programs/taskwarrior
    ./programs/tex
    ./programs/timeular
    ./programs/tmux
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
