{ pkgs, ... }:

let
  shellFunction = shell: pkgs.runCommand "br.${shell}" { nativeBuildInputs = [ pkgs.broot ]; } ''
    broot --print-shell-function ${shell} > $out
  '';
in {
  config = {
    # home-manager's integration targets its own shell config, which we don't use.
    programs.zsh.interactiveShellInit = "source ${shellFunction "zsh"}";
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
