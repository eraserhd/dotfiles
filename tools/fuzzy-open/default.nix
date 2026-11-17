{ pkgs, config, ... }:

let
  # What window managers float the popup by (see modules/fuzzy-open).
  popupName = "fuzzy-open";

  window = pkgs.writeShellApplication {
    name = "fuzzy-open-window";
    runtimeInputs = with pkgs; [ coreutils fre-pluggo fzf gawk ripgrep ];
    text = builtins.readFile ./fuzzy-open-window.bash;
  };

  # What runs in the popup's window.
  windowCommand = pkgs.writeShellScript "fuzzy-open-window" ''
    # The terminal it opens in may have been started by launchd, whose PATH
    # lacks the plan9 tools which 'plumb' needs.
    export PATH=/run/current-system/sw/bin:$PATH
    exec ${window}/bin/fuzzy-open-window
  '';

  popup = pkgs.writeShellScriptBin "fuzzy-open" ''
    exec ${config.local.terminalPopupCommand} ${popupName} ${windowCommand}
  '';
in {
  config = {
    local.fuzzyOpenCommand = "${popup}/bin/fuzzy-open";

    environment.systemPackages = [ popup ];
  };
}
