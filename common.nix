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
    ./tools/1password
    ./tools/aws
    ./tools/barrier
    ./tools/buildkite
    ./tools/chromium
    ./tools/clojure
    ./tools/cue
    ./tools/freecad
    ./tools/git
    ./tools/gnupg
    ./tools/gnuplot
    ./tools/go
    ./tools/gtypist
    ./tools/hammerspoon
    ./tools/i3
    ./tools/k9s
    ./tools/kakoune
    ./tools/kitty
    ./tools/nats
    ./tools/nix
    ./tools/npm
    ./tools/openscad
    ./tools/pop
    ./tools/postgres
    ./tools/rlwrap
    ./tools/taskwarrior
    ./tools/tex
    ./tools/tmux
    ./tools/x
    ./tools/zsh
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
