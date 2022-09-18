{ options, config, lib, pkgs, ... }:

with lib;
let
  cfg = config.local.loginShell;

  homeDirectory = config.users.users.jfelice.home;
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
            nixos-rebuild switch --use-remote-sudo --flake $HOME/src/dotfiles || return $?
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

      source_if_exists ~/.nix-profile/etc/profile.d/nix.sh
      source ${homeDirectory}/src/dotfiles/bin/private.sh

      alias k=kubectl
    '';

    environment.variables = {
      CLICOLOR = "1";
    };

    environment.systemPackages = with pkgs; [
      add-missing
      bat
      broot
      direnv
      entr
      fzf
      jq
      killall
      man-pages
      nix-prefetch-github
      posix_man_pages
      shellcheck
      silver-searcher
      sqltools
      tableize
      tree
      unzip
      wget
      zip
    ];
  } // (if (builtins.hasAttr "defaultUserShell" options.users)
  then {
    users.defaultUserShell = cfg.package;
    documentation.man.generateCaches = true;
  }
  else {
  });
}
