{ lib, options, ... }:

with lib;
{
  imports = [
    ./browser
    ./dogfood
    ./fonts
    ./kits/3d-printing
    ./kits/brain
    ./kits/pcbs
    ./kits/workstation
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
    ./tools/babashka
    ./tools/bCNC
    ./tools/buildkite
    ./tools/clojure
    ./tools/cue
    ./tools/cura
    ./tools/firefox
    ./tools/freecad
    ./tools/git
    ./tools/gnupg
    ./tools/gnuplot
    ./tools/go
    ./tools/hammerspoon
    ./tools/i3
    ./tools/inkscape
    ./tools/k9s
    ./tools/kakoune
    ./tools/kicad
    ./tools/kitty
    ./tools/nats
    ./tools/nix
    ./tools/npm
    ./tools/openscad
    ./tools/pcb2gcode
    ./tools/pop
    ./tools/postgres
    ./tools/rlwrap
    ./tools/synergy
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
