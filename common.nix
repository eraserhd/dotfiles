{ config, lib, options, ... }:

with lib;
{
  imports = [
    ./browser
    ./dogfood
    ./fonts
    ./kits/3d-printing
    ./kits/brain
    ./kits/collaboration
    ./kits/cnc
    ./kits/develop
    ./kits/pcbs
    ./kits/thinking
    ./kits/workstation
    ./modules/bubbles
    ./modules/environment/xdg
    ./networking
    ./networking/ses-email
    ./networking/update-dns
    ./networking/wifi
    ./os/common
    ./pkg
    ./tools/1password
    ./tools/anki
    ./tools/audacity
    ./tools/aws
    ./tools/babashka
    ./tools/bCNC
    ./tools/claude
    ./tools/clojure
    ./tools/cue
    ./tools/discord
    ./tools/elixir
    ./tools/firefox
    ./tools/freecad
    ./tools/git
    ./tools/gnupg
    ./tools/gnuplot
    ./tools/go
    ./tools/gtypist
    ./tools/hammerspoon
    ./tools/inkscape
    ./tools/k9s
    ./tools/kakoune
    ./tools/kicad
    ./tools/kitty
    ./tools/meshlab
    ./tools/minicom
    ./tools/nats
    ./tools/nix
    ./tools/npm
    ./tools/obsidian
    ./tools/openscad
    ./tools/pcb2gcode
    ./tools/polybar
    ./tools/postgres
    ./tools/prusa-slicer
    ./tools/qemu
    ./tools/R
    ./tools/rlwrap
    ./tools/ssh
    ./tools/signal
    ./tools/slack
    ./tools/tex
    ./tools/tmux
    ./tools/tomat
    ./tools/x
    ./tools/xmonad
    ./tools/zoom
    ./tools/zsh
    ./shell
  ];

  config = mkMerge [
    (if (builtins.hasAttr "systemPath" options.environment)
     then {
       environment.systemPath = [ (toString ./bin) ];
     }
     else {
       environment.variables.PATH = [ (toString ./bin) ];
     })

    {
      home-manager.useGlobalPkgs = true;
      home-manager.useUserPackages = true;

      # System secrets are decrypted during activation, which on NixOS happens
      # before /home is mounted, so the host key has to be able to read them.
      # jfelice's key is listed too: it works on darwin, where /Users is always
      # available, and at switch time on NixOS.  Unreadable identities are
      # skipped.
      age.identityPaths = [
        "/etc/ssh/ssh_host_ed25519_key"
        "${config.users.users.jfelice.home}/.ssh/id_ed25519"
      ];
    }

    # A NixOS host whose key isn't a recipient decrypts its secrets fine on
    # `nixos-rebuild switch` (with jfelice's key) and then fails to decrypt
    # them on the next boot, which is a miserable way to find out.
    (optionalAttrs (!(builtins.hasAttr "launchd" options)) {
      assertions = [
        {
          assertion = config.age.secrets == { }
            || (import ./hosts/keys.nix) ? ${config.networking.hostName};
          message = ''
            No host key for '${config.networking.hostName}' in hosts/keys.nix,
            but it has system-level agenix secrets, which are decrypted before
            /home is mounted.  Add it:

                ssh ${config.networking.hostName} cat /etc/ssh/ssh_host_ed25519_key.pub
                agenix -r
          '';
        }
      ];
    })
  ];
}
