{ options, config, lib, pkgs, ... }:

with lib;
let
  cfg = config.local.loginShell;
in {
  options = {
    local.systemDisplayName = mkOption {
      type = types.str;
      description = ''
        System name to display in prompts (can be different from hostname).
      '';
    };
    local.loginShell.package = mkOption {
      type = types.package;
      default = pkgs.bashInteractive;
      description = ''
        Package containing user shell.
      '';
      example = "pkgs.bashInteractive";
    };
  };

  config = {
    environment.interactiveShellInit = ''
      :r() {
        if command -v darwin-rebuild >/dev/null; then
          pushd ~/src/dotfiles >/dev/null
          darwin-rebuild build || return $?
          if [ "$(readlink /run/current-system)" != "$(readlink ./result)" ]; then
            darwin-rebuild switch || return $?
          fi
          popd >/dev/null
        elif command -v nixos-rebuild >/dev/null; then
          pushd ~/src/dotfiles >/dev/null
          nixos-rebuild build || return $?
          if [ "$(readlink /run/current-system)" != "$(readlink ./result)" ]; then
            sudo nixos-rebuild switch || return $?
          fi
          popd >/dev/null
        fi
        unset __NIX_DARWIN_SET_ENVIRONMENT_DONE __NIXOS_SET_ENVIRONMENT_DONE
        unset __ETC_BASHRC_SOURCED __ETC_PROFILE_SOURCED __ETC_PROFILE_DONE
        unset __ETC_ZSHENV_SOURCED __ETC_ZSHRC_SOURCED
        exec '${pkgs.zsh}${pkgs.zsh.shellPath}' -l
      }

      source_if_exists() {
        if [ -f "$1" ]
        then
          source "$1"
        fi
      }

      man() {
          if (( $# == 2 )); then
              kak -e "man $2($1)"
              return $?
          else
              kak -e "man $*"
              return $?
          fi
      }

      ssh() {
          if [[ "$TERM" = "tmux-256color" ]]; then
              TERM=screen command ssh "$@"
              return $?
          else
              command ssh "$@"
              return $?
          fi
      }

      source_if_exists ~/.nix-profile/etc/profile.d/nix.sh
      source ${toString ../bin/private.sh}

      if [ -n "$ZSH_VERSION" ]; then
        eval "$(${pkgs.broot}/bin/broot --print-shell-function zsh)"
      elif [ -n "$BASH_VERSION" ]; then
        eval "$(${pkgs.broot}/bin/broot --print-shell-function bash)"
      fi
    '';

    environment.variables = {
      CLICOLOR = "1";
    };

    environment.systemPackages = with pkgs; [
      add-missing
      ag
      broot
      direnv
      file
      jq
      killall
      posix_man_pages
      shellcheck
      wget
    ];
  } // (if (builtins.hasAttr "defaultUserShell" options.users)
  then {
    users.defaultUserShell = cfg.package;
  }
  else {
  });
}
