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
        local readlink_bin="${pkgs.coreutils}/bin/readlink"
        if command -v darwin-rebuild >/dev/null; then
          pushd ~/src/dotfiles >/dev/null
          TERM=xterm darwin-rebuild build --flake $HOME/src/dotfiles || return $?
          if [ "$($readlink_bin -f /run/current-system)" != "$($readlink_bin -f ./result)" ]; then
            TERM=xterm VERBOSE=1 darwin-rebuild switch --flake $HOME/src/dotfiles || return $?
          fi
          popd >/dev/null
        elif command -v nixos-rebuild >/dev/null; then
          pushd ~/src/dotfiles >/dev/null
          nixos-rebuild build --flake $HOME/src/dotfiles || return $?
          if [ "$($readlink_bin -f /run/current-system)" != "$($readlink_bin -f ./result)" ]; then
            sudo nixos-rebuild switch --flake $HOME/src/dotfiles || return $?
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

      source_if_exists ~/.nix-profile/etc/profile.d/nix.sh
      source ${toString ../bin/private.sh}

      eval "$(2u shell initialize --zsh)"
      eval "$(broot --print-shell-function zsh)"
    '';

    environment.variables = {
      CLICOLOR = "1";
    };

    environment.systemPackages = with pkgs; [
      add-missing
      ag
      bat
      broot
      direnv
      fzf
      jq
      killall
      manpages
      nix-prefetch-github
      posix_man_pages
      shellcheck
      tree
      unzip
      wget
    ];
  } // (if (builtins.hasAttr "defaultUserShell" options.users)
  then {
    users.defaultUserShell = cfg.package;
    documentation.man.generateCaches = true;
  }
  else {
  });
}
