{ lib, options, ... }:

with lib;
{
  imports = [
    ./browser
    ./dogfood
    ./fonts
    ./kits/3d-printing
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
    ./programs/aws
    ./programs/barrier
    ./programs/buildkite
    ./programs/chromium
    ./programs/clojure
    ./programs/cue
    ./programs/freecad
    ./programs/git
    ./programs/gnupg
    ./programs/gnuplot
    ./programs/go
    ./programs/gtypist
    ./programs/hammerspoon
    ./programs/i3
    ./programs/k9s
    ./programs/kakoune
    ./programs/kitty
    ./programs/nats
    ./programs/nix
    ./programs/npm
    ./programs/openscad
    ./programs/pop
    ./programs/postgres
    ./programs/rlwrap
    ./programs/taskwarrior
    ./programs/tex
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
