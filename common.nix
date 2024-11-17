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
    ./tools/anki
    ./tools/aws
    ./tools/babashka
    ./tools/barrier
    ./tools/bCNC
    ./tools/buildkite
    ./tools/clang
    ./tools/clojure
    ./tools/coscreen
    ./tools/cue
    ./tools/discord
    ./tools/firefox
    ./tools/git
    ./tools/gnupg
    ./tools/gnuplot
    ./tools/go
    ./tools/gtypist
    ./tools/hammerspoon
    ./tools/i3
    ./tools/inkscape
    ./tools/k9s
    ./tools/kakoune
    ./tools/kicad
    ./tools/kitty
    ./tools/meshlab
    ./tools/nats
    ./tools/nix
    ./tools/npm
    ./tools/openscad
    ./tools/postman
    ./tools/pcb2gcode
    ./tools/postgres
    ./tools/prusa-slicer
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

    {
      home-manager.useGlobalPkgs = true;
      home-manager.useUserPackages = true;
    }
  ];
}
