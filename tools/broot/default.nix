{ pkgs, config, ... }:

let
  shellFunction = shell: pkgs.runCommand "br.${shell}" { nativeBuildInputs = [ pkgs.broot ]; } ''
    broot --print-shell-function ${shell} > $out
  '';

  # Used by the C-w <space> popup (see tools/aerospace).  A conf of its own
  # since Enter plumbs and leaves here, but only plumbs in the normal broot.
  popupConf = (pkgs.formats.toml {}).generate "broot-popup.toml" {
    default_flags = "-g";
    verbs = [
      {
        invocation = "open_leave_popup";
        key = "enter";
        apply_to = "file";
        execution = "9 plumb {file}";
        leave_broot = true;
      }
    ];
  };

  brootPopup = pkgs.writeShellScriptBin "broot-popup" ''
    # The kitty it opens in may have been started by launchd, whose PATH lacks
    # the plan9 tools which 'plumb' needs, and whose environment lacks the
    # token for the NATS server which the plumber is on.
    export PATH=/run/current-system/sw/bin:$PATH
    ${config.plugbench.tokenPrefix}
    export NATS_TOKEN
    cd "$HOME/src"
    exec ${pkgs.broot}/bin/broot --conf ${popupConf} "$@"
  '';
in {
  config = {
    environment.systemPackages = [ brootPopup ];

    # home-manager's integration does zsh, but our bash is the system's.
    programs.bash.interactiveShellInit = "source ${shellFunction "bash"}";

    home-manager.users.jfelice = { ... }: {
      programs.broot = {
        enable = true;
        settings.default_flags = "-g";
        # Shadow the builtins, which use macOS `open`.
        settings.verbs = [
          {
            invocation = "open_stay";
            shortcut = "os";
            key = "enter";
            apply_to = "file";
            execution = "9 plumb {file}";
            leave_broot = false;
          }
          {
            invocation = "open_leave";
            shortcut = "ol";
            key = "alt-enter";
            apply_to = "file";
            execution = "9 plumb {file}";
            leave_broot = true;
          }
        ];
      };
    };
  };
}
