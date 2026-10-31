{ pkgs, config, lib, ... }:

let
  isDarwin = pkgs.stdenv.hostPlatform.isDarwin;

  # What window managers float the popup by (see modules/fuzzy-open).
  popupName = "fuzzy-open";

  window = pkgs.writeShellApplication {
    name = "fuzzy-open-window";
    runtimeInputs = with pkgs; [ coreutils fre-pluggo fzf gawk git gnused ripgrep ];
    text = builtins.readFile ./fuzzy-open-window.bash;
  };

  # What runs in the popup's window.
  windowCommand = pkgs.writeShellScript "fuzzy-open-window" ''
    # The terminal it opens in may have been started by launchd, whose PATH
    # lacks the plan9 tools which 'plumb' needs, and whose environment lacks
    # the token for the NATS server which the plumber is on.
    export PATH=/run/current-system/sw/bin:$PATH
    ${config.plugbench.tokenPrefix}
    export NATS_TOKEN
    exec ${window}/bin/fuzzy-open-window "$@"
  '';

  # The popup is the focused window once it opens, so what to open in is found
  # before it does.  Only done on macOS, where it is found through AeroSpace.
  popupRoot = pkgs.writeShellApplication {
    name = "fuzzy-open-root";
    runtimeInputs = [ pkgs.coreutils pkgs.git pkgs.jq ];
    text = builtins.readFile ./fuzzy-open-root.bash;
  };

  # Opens the popup, in the directory given if any.
  popup = pkgs.writeShellApplication {
    name = "fuzzy-open";
    runtimeInputs = lib.optional isDarwin popupRoot;
    text = ''
      ${lib.optionalString isDarwin ''
        if [ $# -eq 0 ]; then
          root=$(fuzzy-open-root)
          [ -z "$root" ] || set -- "$root"
        fi
      ''}
      exec ${config.local.terminalPopupCommand} ${popupName} ${windowCommand} "$@"
    '';
  };
in {
  config = {
    local.fuzzyOpenCommand = "${popup}/bin/fuzzy-open";

    environment.systemPackages = [ popup ];
  };
}
